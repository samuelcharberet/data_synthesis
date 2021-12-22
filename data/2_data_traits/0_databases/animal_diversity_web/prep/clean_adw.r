################################ NETSTO PROJECT ################################

# This script extracts trait data from the ADW database (Animal diversity web) 
# authors : Jerome Mathieu, Samuel Charberet
# last update : 19/07/2021
### "https://animaldiversity.ummz.umich.edu/quaardvark/search/"

library(rvest)
library(dplyr)
options("scipen" = 100) # Exponential notation 
library(readr)
library(taxize)


setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/1_tocheck/prep/sources/adw/")
source("C:/Users/Samuel/Dropbox/NetSto/0_database/4_data_analysis/0_functions/gbif_taxo.r")
# on the site you have to choose the features to load:
# in the "search & report" section, choose the species and traits to extract, submit, and copy the url
# NB: possibility of combining a search with as many species and traits as you want
# be careful, some traits include errors when extracting data. ex: basal_metabolic_rate


###### 0. Downloading the database #####

		adw_raw <- NULL

		# 200 results/page -> we navigate from page to page - you must indicate the max number of sp multiple of 200
			for (page in seq(1, 6300, by = 200)) {

				# body mass, primary diet keywords + sep cols, animal foods keywords + sep cols, plant foods keywords + sep cols, other foods keywords + sep cols,  endo, ecto hetero homoithermic sep cols
					mon_url <- paste0("https://animaldiversity.ummz.umich.edu/quaardvark/search/1E2676DC-CC30-0001-D993-2930157BE100/?start=", page,"")
			  	
			  	# request with data fuzzy
			  		#mon_url <- paste0("https://animaldiversity.ummz.umich.edu/quaardvark/search/1E2670F0-7C45-0001-35F1-106012771408/?start=", page,"")

			  page_html <- read_html(mon_url)
			  my_table <- html_table(page_html, fill = TRUE, h=T)[[1]]
			  adw_raw <- bind_rows(adw_raw,my_table)
			  Sys.sleep(1)
			  cat(page,"\n")
			}

		dim(adw_raw) # 6369 69

		adw_raw_backup <- adw_raw
		# adw_raw <- adw_raw_backup



###### 1. Cleaning the database  #####
		
		# columns name
				names(adw_raw) <- tolower(names(adw_raw))
				names(adw_raw) <- gsub("species","species_raw", names(adw_raw))
				names(adw_raw) <- gsub(" ", "_", names(adw_raw)) # reiterate for changes
				names(adw_raw) <- gsub("_-_","-", names(adw_raw))
				names(adw_raw) <- gsub("::",":", names(adw_raw))
				names(adw_raw) <- gsub("_:_|:_", ":", names(adw_raw))
				names(adw_raw) <- gsub(",", "", names(adw_raw))
				names(adw_raw) <- gsub("-", "_", names(adw_raw))
				

		# species names
				adw_raw$species <- tolower(adw_raw$species_raw)
				adw_raw$species <- gsub(" ", "_", adw_raw$species)				


		# columns with keywords

				clean_keywords <- function(x){
						x <- gsub("\\s{2,}", " , ", x)
						x <- gsub(" ::", ":", x)
						x <- gsub(" ,", ",", x)
						return(x)
					}

				keywords_cols <- c("primary_diet", "animal_foods", "plant_foods", "other_foods")

				adw_raw[,keywords_cols] <- sapply(adw_raw[,keywords_cols], clean_keywords) # We apply the same cleaning routine to a subset of columns

				head(adw_raw,20)


		# convert YES to 1
			adw_raw[adw_raw == "YES"] <- 1	


		# clean names of columns	
	  		#colnames(adw) <- gsub("mass_average_kg", "bodymass_kg", colnames(adw))
	  		#colnames(adw) <- gsub("length_average_m", "bodylength_m", colnames(adw))


###### 3.  I/O to backcup and put characters as numeric  #####
			

		write.table(adw_raw, ".\\prep\\adw_raw.csv",sep=",",row.names=FALSE)
		adw_raw <- read.csv(".\\prep\\adw_raw.csv")



###### 4.  Remove irrelevant rows and columns  #####
		
		#  rows with no data

				# function to count the number of NA or empty values in a line
					count_pbs <- function(x){
						test <- length(which(x=="")) + sum(is.na(x))  #== ncol(x)
						return(test)
					}

				# lines for which the nb of pb is not equal to the nb of cols -> not empty lines
					idx_ok <- apply(adw_raw,1,count_pbs) != ncol(adw_raw)- 2	# -2 to take into account the species column


			# keep only non empty lines and back into new df
				adw <- adw_raw[idx_ok,]

		dim(adw) # 3672 70




	  	# Ininteresting columns

	  		adw$bodymass_kg <- adw$mass_average_g/1000
	  		
	  		adw[,c("mass_average_g","mass_adult_average_g","mass_adult_female_average_g","mass_adult_male_average_g","mass_embryo_average_g", "mass_weanling_average_g","mass_female_average_g","mass_male_average_g")] <- NULL
			dim(adw) # 3672 63

			
###### 5. ADW column traceability  #####
			
		names(adw) <- paste0("adw_", names(adw)) # add a prefix in front of all columns names
		names(adw) <- gsub("adw_species","species", names(adw))  # remove the prefix from column containing "species"
		names(adw) <- gsub("adw_bodymass","bodymass", names(adw)) # remove the prefix from column containing "bodymass"
 

###### 5.  I/O to backcup and put characters as numeric  #####
		write.table(adw, ".\\prep\\adw_tmp.csv",sep=",",row.names=FALSE)
		adw <- read.csv(".\\prep\\adw_tmp.csv")

		head(adw)
		summary(adw)
	

###### 6. Get GBIF (Global Biodiversity Information Facility) taxonomy of species  #####

	table_species_gbif <- gbif_taxo(adw$species_raw) # Retrieves the GBIF backbone taxon ID for all species in the ADW database
	dim(table_species_gbif)


	# I/O --
  		write.table(table_species_gbif, ".\\prep\\table_species_gbif.csv",sep=",",row.names=FALSE)
  		#table_species_gbif <- read.csv(".\\prep\\table_species_gbif.csv") ; dim(table_species_gbif)
  		

  	# subset of columns
 		table_species_gbif_s <- cbind(	accepted_species_gbif = table_species_gbif$species,
				accepted_gbif_id = table_species_gbif$specieskey,
				gbif_id = table_species_gbif$usagekey,
				order = table_species_gbif$order,
				family = table_species_gbif$family,
				genus = table_species_gbif$genus,
				status = table_species_gbif$status
				)


###### 7. merge adw data and gbif taxonomy  #####
 		
	complete_adw <- cbind(table_species_gbif_s, adw)
	head(complete_adw)

	# remove rows with unidentified species
			complete_adw <- complete_adw[!is.na(complete_adw$species_raw),]


###### 8. Write a CSV file  #####
			
	write.table(complete_adw, "adw_gbif.csv",sep=",",row.names=FALSE)

	#write.table(useful_data_adw, "gbif_id_adw.csv",sep=",",row.names=FALSE)




