library(taxize)
library(readr)
library(rvest)
library(plyr)
library(dplyr)
library(rgbif)
options( "scipen"=100)


setwd("D:\\Dropbox\\en_cours\\recherche\\projets\\NetSto\\database\\traits\\TOCHECK\\prep\\sources\\elton\\prep")
source("D:\\Dropbox\\en_cours\\recherche\\projets\\NetSto\\0_database\\0_functions\\gbif_taxo.r")

# load data
	mamms <- read.table(".\\clean\\MamFuncDat.txt", sep ="\t", h=T) ; dim(mamms)
	birds <- read_delim(".\\clean\\BirdFuncDat.txt", "\t", escape_double = FALSE, trim_ws = TRUE) ; dim(birds)

	# fix names issues
		birds[birds$species == "Thaumatibis gigantea","species"] <- "Pseudibis gigantea" # giant ibis !
		birds[birds$species == "Oenanthe bifasciata","species"] <- "Campicoloides bifasciatus" # giant ibis !		


# CONCATENATION VERTICALE DES TABLES BIRDS ET MAMMALS

	elton <- rbind.fill(mamms,birds) 

		dim(elton) # 15393 48
		names(elton)

	# select variables of interest
		elton <- elton[, setdiff(colnames(elton),c("MSW3_ID","MSWFamilyLatin","Taxo","Diet_EnteredBy","Diet_Source","IOCOrder",
												"Diet_Certainty","ForStrat_Comment","Activity_Source","Activity_Certainty","bodymass_source","bodymass_speclevel","SpecID",
												"ForStrat_Source", "ForStrat_SpecLevel", "ForStrat_EnteredBy","BLFamSequID","BLFamilyLatin" ,"BLFamilyEnglish")) ]
		names(elton) <- tolower(names(elton))
		dim(elton) # 15393 29
		names(elton)
		names(elton)[grep("species",names(elton))] <- "species_raw"
		head(elton)

	# rename variables
		names(elton) <- paste0("elton_", names(elton))
		names(elton) <- gsub("elton_species","species", names(elton))

		elton$bodymass_kg <- elton$elton_bodymass_g/1000
		elton$elton_bodymass_g <- NULL

# GBIF TAXONOMY

	table_species_gbif <- gbif_taxo(elton$species_raw) 

	View(table_species_gbif)

	dim(elton); dim(table_species_gbif)
	tail(elton,20)[,1:5]; 
	tail(table_species_gbif,20)[,1:5]

	# I/O
		write.table(table_species_gbif, "table_species_gbif.csv",sep=",",row.names=FALSE)
		table_species_gbif <- read.csv("table_species_gbif.csv")
		dim(table_species_gbif)
		
  	# subset of columns
		table_species_gbif_s <- cbind(	accepted_species_gbif = table_species_gbif$species,
				accepted_gbif_id = table_species_gbif$specieskey,
				gbif_id = table_species_gbif$usagekey,
				order = table_species_gbif$order,
				family = table_species_gbif$family,
				genus = table_species_gbif$genus,
				status = table_species_gbif$status
				)


# merge elton data & gbif species --

	elton_gbif <- cbind(table_species_gbif_s,elton)
	head(elton_gbif)


# clean species' name
	elton_gbif$species_raw <- tolower(elton_gbif$species_raw)
	elton_gbif$species_raw <- gsub(" ","_",elton_gbif$species_raw)

# remove rows with unidentified specices
	elton_gbif <- elton_gbif[!is.na(elton_gbif$species_raw),]



# I/O ---
	write.table(elton_gbif, "..\\elton_gbif.csv",sep=",",row.names=FALSE)

	