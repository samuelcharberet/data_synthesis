#' read_ead
#' 
#' # A function to extract data from the
# European Amphibians Database
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species

#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_ead = function(path) {
  setwd(path)
  
  ##### 1. Imports the European Amphibians Database database #####
  
  data_ead = read.csv("biodiversity_data_journal-2-e4123-s001.csv", sep =
                        ";")
  
  # Change column names
  
  names(data_ead) = gsub("\\.", "_", names(data_ead))
  
  ##### 2. Add data from the GBIF Backbone Taxonomy #####
  
  data_ead = add_gbif_backbone_taxonomy(dataframe = data_ead, speciescolumn =
                                          "Species")
  
  
  ##### 3. Keep only one row per species #####
  
  # We remove from data_combine rows which did not find corresponding names in data_gbif
  data_ead = data_ead[-which(is.na(data_ead$species_gbif)),]
  
  # Removes EAD taxonomic data to only keep GBIF data
  # Remove data unused in the NetSto project
  data_ead = subset(
    data_ead,
    select = c(
      Body_mass_in_males,
      Body_mass_in_females,
      Adult_body_mass,
      Body_mass_in_females,
      Adult_body_mass,
      Body_mass_in_juveniles,
      Diurnal,
      Nocturnal,
      Both,
      Carnivorous,
      Insectivorous,
      Moluscivorous,
      Cannibalism,
      Herbivorous,
      Detritivorous,
      Carnivorous_1,
      Insectivorous_1,
      Cannibalism_1,
      Moluscivorous_1,
      Herbivorous_1,
      Detritivorous_1,
      Metabolism_rate,
      Mean_dispersal_distance,
      Maximum_dispersal_distance,
      Minimal_dispersal_distance,
      Mean_migration_distance,
      Maximum_migration_distance,
      Minimal_migration_distance,
      canonical_name_gbif,
      rank_gbif,
      status_gbif,
      match_type_gbif,
      species_gbif,
      specieskey_gbif,
      phylum_gbif,
      class_gbif,
      order_gbif,
      family_gbif,
      genus_gbif
    )
  )
  list_sp = unique(data_ead$species_gbif)
  
  # Removes replicates rows
  data_ead_aggregated = distinct(data_ead)
  
  # There are no several lines per species, the data is thus already aggregated
  
  names(data_ead_aggregated) = tolower(names(data_ead_aggregated))
  
  return(data_ead_aggregated)
  
}