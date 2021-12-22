################################ NETSTO PROJECT ################################

# This script merges vertically tables from the NetSto nutrient database
# cleans the database and adds taxonomic data from the Global Biodiversity Information Facility
# authors : Anne-Cécile vain, Lucie Serre, Jérôme Mathieu, Samuel Charberet
# date : 11/08/2021

# Libraries
library(grid)
library(readr)
library(plyr)
library(dplyr)
library(taxize)

# Define the working directory
setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check")
# Asks the list of all directories
dir() 

######  1. Read nutrient data tables #####

  # fichiers xls(x).
  files <-  list.files(pattern = "[0-9]{4}_[0-9]{3}_.*?_[0-9]{4}.xls", recursive = TRUE)
  n_files=length(files) # the number of dataframes
  # We may want to check manually if the number of elements in 'files' matches the number of tables in the database
  list_files <- lapply(files, readxl::read_excel, na="NA")

######  2. Check that all tables have the same structure ###### 

  # 2.1 Do all tables have the same number of column
  
  ncol_database=82 # In the version of (2021/02/22), the database has 82 columns
  list_ncol = lapply(list_files,ncol)
  wrong_ncol = which(list_ncol!=ncol_database, arr.ind = TRUE) # Which data frame does NOT have the correct number of columns ?
  files[wrong_ncol] # the name of the files that does not have the correct number of columns
  
  # 2.2 Do all tables have the same names of columns and in the same order ? 
  list_colnames = lapply(list_files,names) 
  for (i in 1:ncol_database){
    for (j in 1:n_files){
      if (list_colnames[[1]][i]!=list_colnames[[j]][i]) {
        print(files[j]); print(i)
      }
    }
  }

  
######  3. Vertical concatenation of all tables  ###### 

  data_nutrient_literature <- plyr::rbind.fill(list_files)
  
# Add lab original data
  data_nutrient_samples = readxl::read_excel("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/1_data_samples/1030_999_charberet_2020/1030_999_charberet_2020.xlsx", na="NA")
  data_nutrient_samples
  
  
  data_nutrient = plyr::rbind.fill (data_nutrient_literature, data_nutrient_samples)
  data_nutrient = data_nutrient[rowSums(is.na(data_nutrient)) != ncol(data_nutrient), ] # We remove rows containing only NAs
  
  summary(data_nutrient)
  
######  4. Homogenize the nomenclature of the species variables ####

  data_nutrient$species_latin_name_gbif = tolower(data_nutrient$species_latin_name_gbif)
  data_nutrient$species_latin_name_gbif= gsub(" ","_",data_nutrient$species_latin_name_gbif)

###### 5. Check where there are no GBIF_ID in the database ###

  data_nogbif = data_nutrient[which(is.na(data_nutrient$gbif_id)==T), ]
  # check in the original table if needed

##### 6. Check where there are GBIF_ID given to a taxonomic level higher than species ####

  gbif_ids = unique(data_nutrient$gbif_id)
  list_rank = id2name(gbif_ids, db="gbif")
  
  rank_notspecies = which(sapply(list_rank, function(e) !is.element('species', e)))
  not_species_id =as.integer(names(list_rank[rank_notspecies])); not_species_id=not_species_id[-is.na(not_species_id)]

##### 7. Remove data with taxonomic level higher than species ####

  for (i in 1:length(not_species_id)){
    data_nutrient=data_nutrient[-which(data_nutrient$gbif_id==not_species_id[i]), ]
  }

names(data_nutrient)


##### 8. Create a table with taxonomic data for each species  ####

# creates a character object with n element, n the number of unique species name
list_sp <- unique(data_nutrient$species_latin_name_gbif) # There are x different species names in the raw nutrient database
# Get the GBIF taxonomic data for each species from their taxonomic names
# get_gbifid_ is a function to search GBIF's taxonomy
# if an exact match is found, the ID for that match is returned
# If there isn't an exact match, the options are returned in a dataframe, and we have to pick up one. 
# The match can be either exacvt or fuzzy, and the name can either be synonym or accepted
# Here we create a list of data frames containing one or more match proposals

list_data_gbif = get_gbifid_(list_sp)

# Create a table with taxonomic data for each species
# This table is supposed to have the same number of rows as the number of unique species names in data_nutrient
# We select in priority exact matches and accepted species names, then synonym species names. 
# If there are no exact but only fuzzy matches ,we select in priority accepted species names and then synonym species names

data_gbif <- NULL

for (i in 1:length(list_data_gbif)){
  if (nrow(list_data_gbif[[i]])==1) {
  data_gbif <- bind_rows(data_gbif, list_data_gbif[[i]])
  } else if (nrow(list_data_gbif[[i]])==0) {
  ligne_na=rep(NA, length.out=length(list_data_gbif[[1]]))
  names(ligne_na) = names(list_data_gbif[[1]]);
  data_gbif = bind_rows(data_gbif, ligne_na)
  } else {
  accepted_rows = which(list_data_gbif[[i]]$status=="ACCEPTED")
  synonym_rows = which(list_data_gbif[[i]]$status=="SYNONYM")
  exact_rows = which(list_data_gbif[[i]]$matchtype=="EXACT")
  fuzzy_rows = which(list_data_gbif[[i]]$matchtype=="FUZZY")
    if (length(intersect(accepted_rows, exact_rows))>0) {
    max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence==max(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence)[1])
    data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ]) }
    else if (length(intersect(synonym_rows, exact_rows))>0) {
    max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence==max(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence)[1])
    data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ]) }
    else if (length(intersect(accepted_rows, fuzzy_rows))>0) {
    max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence==max(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence)[1])
    data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ]) }
    else if (length(intersect(synonym_rows, fuzzy_rows))>0) {
    max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence==max(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence)[1])
    data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ]) } 
  }
}

factor_columns = c("rank", "status", "matchtype")
data_gbif[factor_columns] = lapply(data_gbif[factor_columns], factor)


##### 9. Check the taxonomic rank of each line ####
levels(data_gbif$rank) # If possible, aim at only having "species"
subspecies = data_gbif[which(data_gbif$rank=="subspecies"), "canonicalname"] # find subspecies canonical names
# If subspecies is empty, then it means that there are no subspecies in the dataset
subspecies = as.character(subspecies)
subspecies = tolower(subspecies)
subspecies =  gsub(" ","_",subspecies)
unique(data_nutrient[which(data_nutrient$species_latin_name_gbif==subspecies),]$reference_ID) # find references 
# Replace in the table associated with these references ID to put back the species names instead of subspecies

##### 10. Check the status of the scientific name  ####
levels(data_gbif$status) # If possible, aim at only having "ACCEPTED"
synonym = data_gbif[which(data_gbif$status=="SYNONYM"),]$canonicalname # find synonym species canonical names
# If synonym is empty, then it means that there are no synonym species in the dataset
synonym = as.character(synonym)
synonym = tolower(synonym)
synonym =  gsub(" ","_",synonym)
unique(data_nutrient[which(data_nutrient$species_latin_name_gbif==synonym),]$reference_ID) # find references 

##### 11. Check the match types ####
levels(data_gbif$matchtype) # If possible, aim at only having "EXACT"
fuzzy = data_gbif[which(data_gbif$matchtype=="FUZZY"),]$canonicalname # find fuzzy matches canonical names
# If fuzzy is empty, then it means that there are no synonym species in the dataset
fuzzy = as.character(fuzzy)
fuzzy = tolower(fuzzy)
fuzzy =  gsub(" ","_",fuzzy)
list_data_gbif$names
unique(data_nutrient[which(data_nutrient$species_latin_name_gbif==fuzzy),]$reference_ID) # find references 


character_columns = c("rank", "status", "matchtype")
data_gbif[character_columns] = lapply(data_gbif[character_columns], as.character)

##### 12. Check that no GBIF species keys present in data nutrient and absent from data gbif ####
data_no_match = NULL

for (i in 1:nrow(data_nutrient)){
  if (!is.na(data_nutrient$gbif_id[i])) {
  a=data_nutrient$gbif_id[i]==data_gbif$specieskey
    if (length(which(a==T))==0){
    data_no_match = rbind(data_no_match, data_nutrient[ i,])
    }
  }
}
if (is.null(data_no_match)){print("There are no GBIF species key present in data_nutrient and absent from data_gbif")}

#### 13. Combine the GBIF data with the nutrient data  ####

# select relevant data from the GBIF data
useful_data_gbif =  data_gbif[c("canonicalname","rank","status","matchtype","species","specieskey","phylum","class", "order","family",  "genus")]
colnames(useful_data_gbif) <- c("canonical_name","rank","status","match_type","species","specieskey","phylum","class", "order", "family", "genus")

# Add columns to the data_nutrient table
data_nutrient[, c("canonical_name","rank","status","match_type","species","phylum","class", "order", "family","genus")]=NA

# A for loop to combine the GBIF data with the nutrient data

for (i in 1:nrow(data_nutrient)){
  if (!is.na(data_nutrient$gbif_id[i])){
    data_nutrient[i , c("canonical_name","rank","status","match_type","species",
                        "gbif_id","phylum","class", "order", "family","genus")] = 
      useful_data_gbif[which(useful_data_gbif$specieskey==data_nutrient$gbif_id[i]), 
                       c("canonical_name","rank","status","match_type","species",
                         "specieskey","phylum","class", "order", "family","genus")]
  }
}


######  14. Write a data file ######

  setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/2_data_analysis")
  write.csv(data_nutrient,'data_nutrient.csv', row.names = FALSE)
