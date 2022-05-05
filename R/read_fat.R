#' data_fat
#' # A function to extract data from the
# Functional Arthropod Traits (FAT)
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_fat = function(path) {
  setwd(path)
  
  ##### 1. Imports the Functional Arthropod Traits (FAT) database #####
  
  data_fat = read.delim(
    "C:/Users/Samuel/Dropbox/NetSto/0_database/2_data_traits/0_databases/1_done/gossner_arthropods/ArthropodSpeciesTraits.txt"
  )
  
  
  ##### 2. Add data from the GBIF Backbone Taxonomy #####
  
  data_fat = add_gbif_backbone_taxonomy(dataframe = data_fat, speciescolumn =
                                          "SpeciesID")
  
  
  ##### 3. Keep only one row per species #####
  
  # We remove from data_fat rows which did not find corresponding names in data_gbif
  data_fat = data_fat[-which(is.na(data_fat$species_gbif)),]
  
  # Removes FAT taxonomic data to only keep GBIF data
  # Remove data unused in the NetSto project
  data_fat = subset(
    data_fat,
    select = -c(
      Order,
      Suborder,
      Family,
      SpeciesID,
      Author,
      Stratum_use,
      Stratum_use_short
    )
  )
  
  list_sp = unique(data_fat$species_gbif)
  
  # Removes replicates rows
  data_fat_aggregated = distinct(data_fat)
  
  # There are no several lines per species, the data is thus already aggregated
  
  names(data_fat_aggregated) = tolower(names(data_fat_aggregated))
  
  return(data_fat_aggregated)
  
}
