#' read_combine
#'
#' @param path 
#' # A function to extract data from the
# COalesced Mammal dataBase of INtrinsic and Extrinsic traits database (COMBINE)
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species

#'
#' @return
#' @export
#'
#' @examples
read_combine = function(path) {
  setwd(path)
  
  ##### 1. Imports the COMBINE database #####
  
  data_combine = read.csv("trait_data_reported.csv")
  
  # Clean column names
  names(data_combine) = c(
    "order",
    "family",
    "genus",
    "species",
    "iucn2020_binomial",
    "phylacine_binomial",
    "adult_mass",
    "adult_brain_mass",
    "adult_body_length",
    "adult_forearm_length",
    "max_longevity",
    "maturity",
    "female_maturity",
    "male_maturity",
    "age_first_reproduction",
    "gestation_length",
    "teat_number_n",
    "litter_size_n",
    "litters_per_year_n",
    "interbirth_interval",
    "neonate_mass",
    "weaning_age",
    "weaning_mass",
    "generation_length",
    "dispersal",
    "density",
    "hibernation_torpor",
    "fossoriality",
    "home_range",
    "social_group_n",
    "dphy_invertebrate",
    "dphy_vertebrate",
    "dphy_plant",
    "det_inv",
    "det_vend",
    "det_vect",
    "det_vfish",
    "det_vunk",
    "det_scav",
    "det_fruit",
    "det_nect",
    "det_seed",
    "det_plant_other",
    "det_diet_breadth_n",
    "trophic_level",
    "foraging_stratum",
    "activity_cycle",
    "fresh_water",
    "marine",
    "terrestrial_non_volant",
    "terrestrial_volant",
    "upper_elevation",
    "lower_elevation",
    "altitude_breadth",
    "island_dwelling",
    "island_endemicity",
    "disected_by_mountains",
    "glaciation",
    "biogeographical_realm",
    "habitat_breadth_n"
  )
  
  data_combine$adult_mass_unit = "g"
  data_combine$adult_brain_mass_unit = "g"
  data_combine$adult_body_length_unit = "mm"
  data_combine$adult_forearm_length_unit = "mm"
  data_combine$max_longevity_unit = "d"
  data_combine$maturity_unit = "d"
  data_combine$female_maturity_unit = "d"
  data_combine$male_maturity_unit = "d"
  data_combine$age_first_reproduction_unit = "d"
  data_combine$gestation_length_unit = "d"
  data_combine$interbirth_interval_unit = "d"
  data_combine$neonate_mass_unit = "g"
  data_combine$weaning_age_unit = "d"
  data_combine$generation_length_unit = "d"
  data_combine$dispersal_unit = "km"
  data_combine$density_unit = "n/km2"
  data_combine$home_range_unit = "km2"
  data_combine$upper_elevation_unit = "m"
  data_combine$lower_elevation_unit = "m"
  data_combine$altitude_breadth_unit = "m"
  
  
  ##### 2. Filling species names #####
  
  # Check that for all lines, there at least one scientific name, coming either from phylacine, or the IUCN.
  pylacine_nr = which(data_combine$phylacine_binomial == "Not recognised")
  iucn_nr = which(data_combine$iucn2020_binomial == "Not recognised")
  intersect(pylacine_nr, iucn_nr)
  
  # Build a column with IUCN name when Phylacine name is lacking
  data_combine$species_name_all_sources = data_combine$phylacine_binomial
  for (i in 1:nrow(data_combine)) {
    if (data_combine$phylacine_binomial[i] == "Not recognised") {
      data_combine$species_name_all_sources[i] = data_combine$iucn2020_binomial[i]
    }
    else if (data_combine$iucn2020_binomial[i] == "Not recognised") {
      data_combine$species_name_all_sources[i] = data_combine$phylacine_binomial[i]
    }
  }
  if (length(which(is.na(
    data_combine$species_name_all_sources
  ))) > 0) {
    data_combine = data_combine[-which(is.na(data_combine$species_name_all_sources)),]
  }
  
  
  ##### 3. Add data from the GBIF Backbone Taxonomy #####
  
  data_combine = add_gbif_backbone_taxonomy(dataframe = data_combine, speciescolumn =
                                              "species_name_all_sources")
  
  
  ##### 4. Keep only one row per species #####
  
  # We remove from data_combine rows which did not find corresponding names in data_gbif
  data_combine = data_combine[-which(is.na(data_combine$species_gbif)),]
  
  # Removes phylacine and IUCN taxonomic data to only keep GBIF data
  
  data_combine = subset(
    data_combine,
    select = -c(
      order,
      family,
      genus,
      species,
      species_name_all_sources,
      iucn2020_binomial,
      phylacine_binomial,
      adult_brain_mass,
      adult_body_length,
      adult_forearm_length,
      max_longevity,
      maturity,
      female_maturity,
      male_maturity,
      age_first_reproduction,
      gestation_length,
      teat_number_n,
      litter_size_n,
      litters_per_year_n,
      interbirth_interval,
      neonate_mass,
      weaning_age,
      weaning_mass,
      generation_length,
      dispersal,
      density,
      hibernation_torpor,
      fossoriality,
      home_range,
      social_group_n,
      activity_cycle,
      fresh_water,
      marine,
      upper_elevation,
      lower_elevation,
      altitude_breadth,
      island_dwelling,
      island_endemicity,
      disected_by_mountains,
      glaciation,
      habitat_breadth_n,
      adult_brain_mass_unit,
      adult_body_length_unit,
      adult_forearm_length_unit,
      max_longevity_unit,
      maturity_unit,
      female_maturity_unit,
      male_maturity_unit,
      age_first_reproduction_unit,
      gestation_length_unit,
      interbirth_interval_unit,
      neonate_mass_unit,
      weaning_age_unit,
      generation_length_unit,
      dispersal_unit,
      density_unit,
      home_range_unit,
      upper_elevation_unit,
      lower_elevation_unit,
      altitude_breadth_unit,
      dphy_invertebrate,
      dphy_vertebrate,
      dphy_plant
    )
  )
  data_combine$diet_unit = "%"
  list_sp = unique(data_combine$species_gbif)
  
  # Removes replicates rows
  data_combine = distinct(data_combine)
  
  # As we don't want to compute averages of values of different statistical status
  # (averages of unknown n, unique observations) have different weights, we will have to choose one data per species and per variable instead
  # The number of data for each species is very often 1 or 2. It is therefore risky to compute median as it would result in non weighted averages.
  # Therefore for each species, if there are several data for the same variable we keep the data coming from the reference having the most data
  
  list_sp =
    unique(data_combine$species_gbif)
  if (length(which(is.na(list_sp))) > 0) {
    list_sp = list_sp[-which(is.na(list_sp))]
  }
  
  data_combine_aggregated = NULL
  mat_na = 1 * !is.na(data_combine) # Where are data
  col_choice = names(data_combine)[-c(
    which(names(data_combine) == c("canonical_name_gbif")),
    which(names(data_combine) == c("rank_gbif")),
    which(names(data_combine) == c("status_gbif")),
    which(names(data_combine) == c("match_type_gbif")),
    which(names(data_combine) == c("species_gbif")),
    which(names(data_combine) == c("specieskey_gbif")),
    which(names(data_combine) == c("phylum_gbif")),
    which(names(data_combine) == c("class_gbif")),
    which(names(data_combine) == c("order_gbif")),
    which(names(data_combine) == c("family_gbif")),
    which(names(data_combine) == c("genus_gbif"))
  )]
  
  row_data_nb = rep(NA, nrow(data_combine))
  
  for (i in 1:length(list_sp)) {
    lines_species = which(data_combine$species_gbif == list_sp[i])# select the lines of the current species
    if (length(lines_species) == 1) {
      data_combine_aggregated = bind_rows(data_combine_aggregated, data_combine[lines_species,])
    } # if there is only on row for the current species, we keep that row
    else {
      combined_row = data_combine[lines_species[1],]
      
      for (j in col_choice) {
        if (sum(mat_na[lines_species, j]) == 0) {
          combined_row[1, j] = NA
        }
        else if (sum(mat_na[lines_species, j]) == 1) {
          combined_row[1, j] = data_combine[lines_species[which(mat_na[lines_species, j] ==
                                                                  1)], j]
        }
        else if (sum(mat_na[lines_species, j]) > 1) {
          for (k in lines_species) {
            row_data_nb[k] = sum(mat_na[k, col_choice])
          }
          combined_row[1, j] = data_combine[lines_species[which(row_data_nb[lines_species] ==
                                                                  max(row_data_nb[lines_species]))][1], j]
        }
      }
      data_combine_aggregated = bind_rows(data_combine_aggregated, combined_row[1,])
    }
  }
  
  return(data_combine_aggregated)
}