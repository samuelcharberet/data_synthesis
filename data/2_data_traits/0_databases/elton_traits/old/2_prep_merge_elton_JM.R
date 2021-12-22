# Lucie Serre 
# mis a jour le 09.04.2020

# rearrangement en prevision du merge avec adw

library(readr)
library(rvest)
library(dplyr)

setwd("~/Dropbox/dejections/database/traits/TOCHECK/elton_foraging_database_birds&mammals/fichiers_prep")
elton_ready_to_merge <- read_csv("~/Dropbox/dejections/database/traits/TOCHECK/elton_foraging_database_birds&mammals/fichiers_prep/complete_elton.csv")

setwd("D:\\Dropbox\\en_cours\\recherche\\projets\\dejections_2020\\database\\traits\\TOCHECK\\elton_foraging_database_birds&mammals\\fichiers_prep")
elton_ready_to_merge <- read_csv("complete_elton.csv")



# NOM ESPECE SCIENTIFIQUE
	colnames(elton_ready_to_merge) <- gsub("scientific", "species", colnames(elton_ready_to_merge))
	elton_ready_to_merge$species <- tolower(elton_ready_to_merge$species)
	elton_ready_to_merge$species <- gsub(" ","_",elton_ready_to_merge$species)

	### dummy variable
		elton_ready_to_merge$diet_5cat_f <- elton_ready_to_merge$diet_5cat
		model.matrix(~ elton_ready_to_merge$diet_5cat_f + 0)


# REGIMES TROPHIQUES (recoder une variable qualitative à 5 modalites en 5 variables binaires)

	elton_ready_to_merge$carnivore_vertebrate <- rep(NA,5616)
	elton_ready_to_merge$carnivore_vertebrate <- replace(elton_ready_to_merge$carnivore_vertebrate, elton_ready_to_merge$diet_5cat == "VertFishScav", 1)
	### il n y a pas de vertfishscav ### !!!

	elton_ready_to_merge$carnivore_invertebrate <- rep(NA,5616)
	elton_ready_to_merge$carnivore_invertebrate <- replace(elton_ready_to_merge$carnivore_invertebrate, elton_ready_to_merge$diet_5cat=="Invertebrate", 1)

	elton_ready_to_merge$herbivore_plantseed <- rep(NA,5616)
	elton_ready_to_merge$herbivore_plantseed <- replace(elton_ready_to_merge$herbivore_plantseed, elton_ready_to_merge$diet_5cat=="PlantSeed",1)

	elton_ready_to_merge$herbivore_fruitnect <- rep(NA,5616)
	elton_ready_to_merge$herbivore_fruitnect <- replace(elton_ready_to_merge$herbivore_fruitnect, elton_ready_to_merge$diet_5cat=="FruiNect", 1)

	elton_ready_to_merge$omnivore <- rep(NA,5616)
	elton_ready_to_merge$omnivore <- replace(elton_ready_to_merge$omnivore,elton_ready_to_merge$diet_5cat=="Omnivore",1)

	elton_ready_to_merge$diet_5cat <- NULL


elton_ready_to_merge$diet_5cat


# REGIMES ALIMENTAIRE (on passe de porcentage a presence/absence 0/1)
	elton_ready_to_merge[,6:15] <- replace(elton_ready_to_merge[,6:15],elton_ready_to_merge[,6:15] > 0, 1)

	colnames(elton_ready_to_merge) <- gsub("diet_inv", "diet_invertebrate", colnames(elton_ready_to_merge))
	colnames(elton_ready_to_merge) <- gsub("diet_vend", "diet_vertebrate_endo", colnames(elton_ready_to_merge))
	colnames(elton_ready_to_merge) <- gsub("diet_vect", "diet_vertebrate_ecto", colnames(elton_ready_to_merge))
	colnames(elton_ready_to_merge) <- gsub("diet_vfish", "diet_vertebrate_fish", colnames(elton_ready_to_merge))
	colnames(elton_ready_to_merge) <- gsub("diet_vunk", "diet_vertebrate_unk", colnames(elton_ready_to_merge))
	colnames(elton_ready_to_merge) <- gsub("diet_planto", "diet_plant", colnames(elton_ready_to_merge))

# bodymass_kg (passer en kg)
	elton_ready_to_merge$bodymass_value <- elton_ready_to_merge$bodymass_value/1000
	colnames(elton_ready_to_merge) <- gsub("bodymass_value", "bodymass_kg", colnames(elton_ready_to_merge))

# activity_nocturnal (regroupement activity_nocturnal et nocturnal)

	elton_ready_to_merge$activity_nocturnal <- replace(elton_ready_to_merge$activity_nocturnal,elton_ready_to_merge$nocturnal==1,1)
	elton_ready_to_merge$activity_nocturnal <- replace(elton_ready_to_merge$activity_nocturnal,elton_ready_to_merge$nocturnal==0,0)

	# ordre des variables
	elton_ready_to_merge <- elton_ready_to_merge[c(1,5,4,2,3,53:57,6:52)]

# I/O
write.table(elton_ready_to_merge, "elton_ready_to_merge.csv",sep=",",row.names=FALSE)


