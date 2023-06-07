#' data_amniota
#' 
#'A function to extract data from the
# Amniota database
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species

#'
#' @param amniota 
#'
#' @return
#' @export
#'
#' @examples
read_amniota = function(amniota) {
  ##### 1. Import Amniota database #####
  
  data_amniota = amniota
  rm(amniota)
  
  # Create a species binomial name column
  data_amniota$binomial = paste(data_amniota$genus, data_amniota$species)
  
  
  ##### 2. Add data from the GBIF Backbone Taxonomy  #####
  
  # A function to download data from the GBIF Backbone Taxonomy and combine
  # it with a given dataframe containing a column with species names
  
  data_amniota = add_gbif_backbone_taxonomy(dataframe = data_amniota, speciescolumn =
                                              "binomial")
  
  
  ##### 4. Keep only one row per species  #####
  
  
  # We remove from data_combine rows which did not find corresponding names in data_gbif
  if (length(which(is.na(data_amniota$species_gbif))) > 0) {
    data_amniota = data_amniota[-which(is.na(data_amniota$species_gbif)),]
  }
  
  
  # Removes data we are not interested in
  data_amniota = subset(
    data_amniota,
    select = c(
      adult_body_mass_g,
      female_body_mass_g,
      male_body_mass_g,
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
  
  # Removes replicates rows
  data_amniota_aggregated = distinct(data_amniota)
  
  # There are no several lines per species, the data is thus already aggregated
  
  return(data_amniota_aggregated)
  
}