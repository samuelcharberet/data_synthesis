# Lucie Serre 
# mis a jour le 09.04.2020

# rearrangement en prevision du merge avec adw

library(readr)
library(rvest)
library(dplyr)

setwd("D:\\Dropbox\\en_cours\\recherche\\projets\\NetSto\\database\\traits\\TOCHECK\\prep\\sources\\elton_foraging_database_birds&mammals\\fichiers_prep")

elton_gbif <- read.csv("elton_gbif.csv")

# clean species' name
	elton_gbif$species <- tolower(elton_gbif$species)
	elton_gbif$species <- gsub(" ","_",elton_gbif$species)

# REGIMES TROPHIQUES (recoder une variable qualitative à 5 modalites en 5 variables binaires)

	diet5 <- fastDummies::dummy_cols(elton_gbif$diet_5cat)
	diet5 <- diet5[,c(".data_FruiNect", ".data_Invertebrate", ".data_Omnivore", ".data_PlantSeed", ".data_VertFishScav")]
	names(diet5) <- gsub(".data_", "", names(diet5))
	names(diet5) <- gsub("VertFishScav", "carnivore_vertebrate", names(diet5))
	names(diet5) <- tolower(names(diet5))

# binarize dummy diets
	elton_gbif[,grep("diet_[a-z]",names(elton_gbif))] <- replace(elton_gbif[,grep("diet_[a-z]",names(elton_gbif))], elton_gbif[,grep("diet_[a-z]",names(elton_gbif))] > 0, 1)

# clean diet names
	names(elton_gbif) <- gsub("diet_inv", "diet_invertebrate", names(elton_gbif))
	names(elton_gbif) <- gsub("diet_vend", "diet_vertebrate_endo", names(elton_gbif))
	names(elton_gbif) <- gsub("diet_vect", "diet_vertebrate_ecto", names(elton_gbif))
	names(elton_gbif) <- gsub("diet_vfish", "diet_vertebrate_fish", names(elton_gbif))
	names(elton_gbif) <- gsub("diet_vunk", "diet_vertebrate_unk", names(elton_gbif))
	names(elton_gbif) <- gsub("diet_planto", "diet_plant", names(elton_gbif))

# bodymass_kg (passer en kg)
	elton_gbif$bodymass_kg <- elton_gbif$bodymass_g/1000


# activity_nocturnal (regroupement "activity_nocturnal" et "nocturnal")
	elton_gbif$activity_nocturnal<-replace(elton_gbif$activity_nocturnal,elton_gbif$nocturnal==1,1)
	elton_gbif$activity_nocturnal<-replace(elton_gbif$activity_nocturnal,elton_gbif$nocturnal==0,0)
	elton_gbif$nocturnal <- NULL


# ordre des variables
elton_gbif <- elton_gbif[c(1,5,4,2,3,53:57,6:52)]

write.table(elton_gbif, "elton_gbif.csv",sep=",",row.names=FALSE)







		elton_gbif$carnivore_vertebrate <- rep(NA,5616)
		elton_gbif$carnivore_vertebrate <- replace(elton_gbif$carnivore_vertebrate, elton_gbif$diet_5cat=="VertFishScav", 1)

		elton_gbif$carnivore_invertebrate <- rep(NA,5616)
		elton_gbif$carnivore_invertebrate <-replace(elton_gbif$carnivore_invertebrate, elton_gbif$diet_5cat=="Invertebrate",1)

		elton_gbif$herbivore_plantseed <- rep(NA,5616)
		elton_gbif$herbivore_plantseed <-replace(elton_gbif$herbivore_plantseed,elton_gbif$diet_5cat=="PlantSeed",1)

		elton_gbif$herbivore_fruitnect <- rep(NA,5616)
		elton_gbif$herbivore_fruitnect <-replace(elton_gbif$herbivore_fruitnect,elton_gbif$diet_5cat=="FruiNect",1)

		elton_gbif$omnivore <- rep(NA,5616)
		elton_gbif$omnivore <- replace(elton_gbif$omnivore,elton_gbif$diet_5cat=="Omnivore",1)

		elton_gbif$diet_5cat <- NULL

		# REGIMES ALIMENTAIRE (on passe de porcentage a presence/absence 0/1)
elton_gbif[,6:15] <- replace(elton_gbif[,6:15],elton_gbif[,6:15]>0,1)
