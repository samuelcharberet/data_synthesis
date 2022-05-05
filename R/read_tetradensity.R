#' data_tetradensity
# A function to extract data from the
# TetraDENSITY database
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
#' 
read_tetradensity = function(path) {
  setwd(path)
  
  ##### 1. Imports the TETRAdensity database #####
  
  data_tetradensity = read.csv("TetraDENSITY_v.1.csv", sep = ",")
  
  # Create a species binomial name column
  data_tetradensity$binomial = paste(data_tetradensity$Genus, data_tetradensity$Species)
  length(unique(data_tetradensity$Species))
  ##### 2. Add data from the GBIF Backbone Taxonomy #####
  
  data_tetradensity = add_gbif_backbone_taxonomy(dataframe = data_tetradensity, speciescolumn =
                                                   "binomial")
  
  
  ##### 3. Keep only one row per species #####
  
  # We remove from data_combine rows which did not find corresponding names in data_gbif
  data_tetradensity = data_tetradensity[-which(is.na(data_tetradensity$species_gbif)), ]
  
  # Removes Open Tree taxonomy data to only keep GBIF taxonomic data and remove data unused in the NetSto project
  names(data_tetradensity)
  data_tetradensity = subset(
    data_tetradensity,
    select = -c(
      Longitude,
      Latitude,
      Locality,
      Country,
      Habitat,
      Class,
      Order,
      Family,
      Genus,
      Species,
      Subspecies,
      Year,
      Season.Month,
      Sampling_Area,
      Sampling_Area_unit,
      binomial
    )
  )
  
  # We remove data that are given in male/ha
  data_tetradensity = data_tetradensity[-which(data_tetradensity$Density_unit ==
                                                 "males/ha"), ]
  
  # We remove data determined by unclear method
  data_tetradensity = data_tetradensity[-which(
    data_tetradensity$Method_info == "Unclear method" |
      data_tetradensity$Method_info == "Unclear method; probably counts or home range extrapolations"
  ), ]
  
  
  # We put all data in the same unit (ind/km2)
  
  for (i in 1:nrow(data_tetradensity)) {
    if (data_tetradensity$Density_unit[i] == "ind/ha") {
      data_tetradensity$Density_unit[i] = "ind/km2"
      data_tetradensity$Density[i] = data_tetradensity$Density[i] / 100
    }
    else if (data_tetradensity$Density_unit[i] == "pairs/km2") {
      data_tetradensity$Density_unit[i] = "ind/km2"
      data_tetradensity$Density[i] = data_tetradensity$Density[i] * 2
    }
    
  }
  
  
  # As we don't want to compute averages of values of different statistical status
  # (averages of unknown n, unique observations) have different weights, we will have to choose one data per species and per variable instead
  # For each species, if there are several densities data for the density
  # we pick the median among the data estimated using the most frequent method
  # and keep only this line of data
  
  list_sp = unique(data_tetradensity$species_gbif)
  
  
  if (length(which(is.na(list_sp))) > 0) {
    list_sp = list_sp[-which(is.na(list_sp))]
  }
  
  data_tetradensity_aggregated = NULL
  col_cont =   names(data_tetradensity)[c(which(names(data_tetradensity) == c("Density")))]
  col_meth = names(data_tetradensity)[c(which(names(data_tetradensity) == c("Sampling_Method")))]
  
  
  for (i in 1:length(list_sp)) {
    lines_species = which(data_tetradensity$species_gbif == list_sp[i])# select the lines of the current species
    if (length(lines_species) == 1) {
      data_tetradensity_aggregated = bind_rows(data_tetradensity_aggregated, data_tetradensity[lines_species, ])
    } else {
      lines_method = lines_species[which(data_tetradensity[lines_species, "Sampling_Method"] == names(sort(
        table(data_tetradensity[lines_species, "Sampling_Method"]), decreasing = TRUE
      )[1]))]# select the lines of the most frequent method
      combined_row = data_tetradensity[lines_method[1],]
      combined_row$Density = median(data_tetradensity[lines_method, "Density"])[1]
      combined_row$Method_info = NA
      data_tetradensity_aggregated = bind_rows(data_tetradensity_aggregated, combined_row)
    }
  }
  
  names(data_tetradensity_aggregated) = tolower(names(data_tetradensity_aggregated))
  
  return(data_tetradensity_aggregated)
  
}