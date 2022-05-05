#' read_amphibio
#' 
#' # A function to extract data from the
# Amphibio database
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species
#'
#' @param amphibio 
#'
#' @return
#' @export
#'
#' @examples
read_amphibio = function(amphibio) {
  ##### 1. Import Amphibio database #####
  
  data_amphibio = amphibio
  rm(amphibio)
  
  
  ##### 3. Add data from the GBIF Backbone Taxonomy  #####
  
  # A function to download data from the GBIF Backbone Taxonomy and combine
  # it with a given dataframe containing a column with species names
  
  
  data_amphibio = add_gbif_backbone_taxonomy(dataframe = data_amphibio, speciescolumn =
                                               "Species")
  
  
  ##### 4. Keep only one row per species  #####
  
  
  # We remove from data_combine rows which did not find corresponding names in data_gbif
  if (length(which(is.na(data_amphibio$species_gbif))) > 0) {
    data_amphibio = data_amphibio[-which(is.na(data_amphibio$species_gbif)),]
  }
  
  names(data_amphibio)
  
  # Removes data we are not interested in
  data_amphibio = subset(
    data_amphibio,
    select = -c(
      id,
      Order,
      Family,
      Genus,
      Species,
      Fos,
      Arb,
      Wet_warm,
      Wet_cold,
      Dry_warm,
      Dry_cold,
      Age_at_maturity_min_y,
      Age_at_maturity_max_y,
      Body_size_mm,
      Size_at_maturity_min_mm,
      Size_at_maturity_max_mm,
      Longevity_max_y,
      Litter_size_max_n,
      Reproductive_output_y,
      Offspring_size_min_mm,
      Offspring_size_max_mm,
      Dir,
      Lar,
      Viv
    )
  )
  
  # Removes replicates rows
  data_amphibio_aggregated = distinct(data_amphibio)
  
  list_sp = unique(data_amphibio$species_gbif)
  # There are no several lines per species, the data is thus already aggregated
  
  names(data_amphibio_aggregated) = tolower(names(data_amphibio_aggregated))
  
  
  return(data_amphibio_aggregated)
  
}