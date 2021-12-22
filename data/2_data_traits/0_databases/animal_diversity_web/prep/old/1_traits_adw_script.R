# Lucie Serre 
# mis a jour le 27.03.2020

# Extraction des données "traits" de la base de données adw
# "https://animaldiversity.ummz.umich.edu"

library(rvest)
library(dplyr)
options("scipen" = 100) # notation exponentielle


setwd("D:\\Dropbox\\en_cours\\recherche\\projets\\NetSto\\database\\traits\\TOCHECK\\prep\\sources\\adw")

# sur le site il faut choisir les traits à charger :
	# dans la section "search & report", choisir les especes et traits à extraire, submit, et copier l'url
	# NB: possibilite de combiner une recherche avec autant d'especes et traits qu'on veut
	# attention certains traits induisent des erreurs. ex: basal_metabolic_rate

traits_adw <- NULL

# 200 results/page -> on navigue de page à page - il faut indiquer le nb max d'sp multiple de 200
	for (page in seq(1,6300,by=200)) {
	  mon_url <- paste0("https://animaldiversity.ummz.umich.edu/quaardvark/search/1E2670F0-7C45-0001-35F1-106012771408/?start=",page,"")
	  #mon_url <- paste0("https://animaldiversity.ummz.umich.edu/quaardvark/search/1E261E47-7FF5-0001-CCC7-B4001B30ECF0/?start=",page,"")

	  page_html <- read_html(mon_url)
	  my_table <- html_table(page_html, fill = TRUE, h=T)[[1]]
	  traits_adw <- bind_rows(traits_adw,my_table)
	  Sys.sleep(1)
	  cat(page)
	}

	dim(traits_adw) #6369

	# clean
		# columns name
			names(traits_adw) <- tolower(names(traits_adw))
			names(traits_adw) <- gsub(" ", "_", names(traits_adw)) # réitérer pour les ≠ changements
		# species names
			traits_adw$species <- tolower(traits_adw$species)
			traits_adw$species <- gsub(" ", "_", traits_adw$species)

		# remove rows with no data

			# function to count the number of na or empty values in a line
				count_pbs <- function(x){
					test <- length(which(x=="")) + sum(is.na(x))  #== ncol(x)
					return(test)
				}

			# lines for which the nb of pb is not equal to the nb of cols -> not empty lines
				idx_ok <- apply(traits_adw,1,count_pbs) != ncol(traits_adw)- 1	# -1 to take into account the species column

			# keep only non empty lines
				traits_adw2 <- traits_adw[idx_ok,]

	dim(traits_adw2) # 3760

traits_adw2 <- tidyr::replace_na(traits_adw2,"")

# NETTOYAGE

	#traits_adw[traits_adw == "YES"] <- 1
	#traits_adw[traits_adw == ""] <- 0

	#traits_adw[,1:64][is.na(traits_adw[,1:64])] <- 0 # seulement pour certaines colonnes 
	#str(traits_adw)

# Ajout manuel de basal_metabolic_rate avec uniquement des NA (aucune donnée dans adw et cause pb)
	#	traits_adw$basal_metabolic_rate
	#	traits_adw$basal_metabolic_rate <- rep(NA,5735)

# mise au propre de la nomenclature et de l'ordre des colonnes

	#traits_adw <- traits_adw[c(1:68,75:79,85,86,69:71,80:84,87,88,72:74,89)]

# I/O - ENRIGISTREMENT TABLE PROPRE
	write.table(traits_adw, "adw_raw.csv",sep=",",row.names=FALSE)
	write.table(traits_adw2, "adw_ok.csv",sep=",",row.names=FALSE)

