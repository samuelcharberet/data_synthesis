#' read_meiri
#' 
#' # A function to extract data from the
# Meiri database : Meiri 2019 - Traits of lizards of the world: variation around a successful evolutionary design
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_meiri = function(path) {
  setwd(path)
  
  ##### 1. Imports the Meiri 2019 database #####
  
  data_meiri = read.csv("Appendix S1 - Lizard data version 1.0.csv")
  
  
  # Change column names
  
  names(data_meiri) = gsub("\\.\\.", "_", names(data_meiri))
  names(data_meiri) = gsub("\\.", "_", names(data_meiri))
  
  data_meiri[which(data_meiri == "", arr.ind = TRUE)] = NA # Put NA where there is nothing in the cell
  data_meiri = data_meiri[rowSums(is.na(data_meiri)) != ncol(data_meiri), ] # Remove lines of only NA
  
  # Remove extinct species
  
  data_meiri = data_meiri[which(data_meiri$Extant_Extinct == "extant"), ]
  
  
  ##### 2. Add data from the GBIF Backbone Taxonomy #####
  
  data_meiri = add_gbif_backbone_taxonomy(dataframe = data_meiri, speciescolumn =
                                            "Binomial")
  
  
  ##### 3. Keep only one row per species #####
  
  # We remove from data_meiri rows which did not find corresponding names in data_gbif
  data_meiri = data_meiri[-which(is.na(data_meiri$species_gbif)),]
  
  # Removes Meiri taxonomic data to only keep GBIF data
  # Remove data unused in the NetSto project
  names(data_meiri)
  data_meiri = subset(
    data_meiri,
    select = -c(
      Binomial,
      Genus,
      epithet,
      valid_reptile_database_February_2018_,
      year_of_description,
      country_described_from,
      main_biogeographic_Realm,
      known_only_from_the_only_type_,
      insular_endemic,
      Leg_development,
      substrate,
      substrate_comments,
      reproductive_mode,
      clutch_size,
      smallest_clutch,
      largest_clutch,
      smallest_mean_clutch_size,
      largest_mean_clutch_size,
      breeding_age_months_,
      youngest_age_at_first_breeding_months_,
      oldest_age_at_first_breeding_months_,
      Family,
      IUCN_redlist_assessment,
      IUCN_population_trend,
      Extant_Extinct,
      Remarks
    )
  )
  list_sp = unique(data_meiri$species_gbif)
  
  # Removes replicates rows
  data_meiri_aggregated = distinct(data_meiri)
  
  # There are no several lines per species, the data is thus already aggregated
  
  names(data_meiri_aggregated) = tolower(names(data_meiri_aggregated))
  
  return(data_meiri_aggregated)
  
}