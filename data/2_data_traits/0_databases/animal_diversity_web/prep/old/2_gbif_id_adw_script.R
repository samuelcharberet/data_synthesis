# Lucie Serre 
# mis a jour le 27.03.2020

# evaluer le statut SYNONYM/ACCEPTED de l'espece extraite de la base adw
# associer les gbif_id aux especes
# NB dans le cas des synonymes, donner le gbif_id de l'espece accepted corresppondante

# en associant les especes adw aux accepted_Gbif_id, plusieurs especes auront le meme accepted_Gbif_id
# risque de poser pb lors de la jointure via gbif !

library(readr)
library(taxize)
library(dplyr)

traits_adw <- read_csv("~/Dropbox/dejections/database/traits/TOCHECK/adw/fichiers_prep/1_traits_adw.csv")
useful_data <- NULL

# étape 1 facultative (si on la saute, penser a changer les arguments)
# correction des eventuelles erreurs orthographiques
# ici inutile car les noms d'especes sont bien orthographies
# mais utile si on veut l'appliquer a une liste d'especes entrées manuellement 

	resolved <- gnr_resolve(traits_adw$species, best_match_only = TRUE, canonical = TRUE)
	resolved

	traits_adw$species[!traits_adw$species%in%resolved$user_supplied_name] # si diff

# étape 2: récupération d'informations sur les especes, dont le statut ACCEPTED/SYNONYM, le gbif_id
# et dans le cas des especes SYNONYMS, l'espece accepted et le gbif accepted correspondants
# dans une liste de data frames, list_table_info chaque data frame contenant une ou plusieurs propositions de matchs

	list_tables_gbif <- get_gbifid_(traits_adw$species)

# pour chaque data frame, sélection du match le plus pertinent (accepted & exact)
# dans certains cas, il s'agit d'une especes synonym, choisir le match exact
# quand ils il y plusieurs matchs accepted exact, prendre le plus pertinent ie celui avec le confidence max ie le 1er
# placer chaque match sélectionné dans une table

	table_gbif <- NULL

	for (i in 1:length(list_tables_gbif)){
	  if (nrow(list_tables_gbif[[i]])==1) table_gbif <- bind_rows(table_gbif, list_tables_gbif[[i]])
	  else 
	    if (list_tables_gbif[[i]]$status == "ACCEPTED"&list_tables_gbif[[i]]$matchtype=="EXACT") 
	      table_gbif <- bind_rows(table_gbif, list_tables_gbif[[i]][list_tables_gbif[[i]]$status == "ACCEPTED"&list_tables_gbif[[i]]$matchtype == "EXACT",][1,])
	    else if (list_tables_gbif[[i]]$status == "SYNONYM"&list_tables_gbif[[i]]$matchtype=="EXACT") 
	      table_gbif <- bind_rows(table_gbif, list_tables_gbif[[i]][list_tables_gbif[[i]]$status == "SYNONYM"&list_tables_gbif[[i]]$matchtype == "EXACT",][1,])
	    else table_gbif <- bind_rows(table_gbif,list_tables_gbif[[i]][1,])
	}

	View(table_gbif)

# selection des données pertinentes dans une table (resolved$matched_name2 facultatif)
	useful_data_adw <- cbind(traits_adw$species, table_gbif[c("canonicalname","usagekey","status","matchtype","species","specieskey","order","family")])
	colnames(useful_data_adw) <- c("mentionned_species","canonical_name","gbif_id","status","match_type","accepted_species","accepted_gbif_id","order","family")


# ajout des accepted_gbif_id à la table de traits
	complete_adw <- cbind(accepted_gbif_id=useful_data_adw$accepted_gbif_id,order=useful_data_adw$order,family=useful_data_adw$family,status=useful_data_adw$status,traits_adw)


# I/O --

	write.table(useful_data_adw, "gbif_id_adw.csv",sep=",",row.names=FALSE)
	write.table(complete_adw, "complete_adw.csv",sep=",",row.names=FALSE)
