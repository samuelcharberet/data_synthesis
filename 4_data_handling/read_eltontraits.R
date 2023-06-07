#' read_eltontraits
#'A function to extract data from the
# EltonTraits database
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species
#'
#' @param eltontraits 
#'
#' @return
#' @export
#'
#' @examples
read_eltontraits = function(eltontraits) {
  
  ##### 1. Import EltonTraits database #####
  
  data_eltontraits = eltontraits
  rm(eltontraits)
  
  ##### 2. Remove mammals data and only keep birds data #####
  
  mammal_indices = which(is.na(data_eltontraits$PassNonPass))
  data_eltontraits = data_eltontraits[-mammal_indices,]
  
  
  ##### 3. Add data from the GBIF Backbone Taxonomy  #####
  
  # A function to download data from the GBIF Backbone Taxonomy and combine
  # it with a given dataframe containing a column with species names
  
  
  data_eltontraits = add_gbif_backbone_taxonomy(dataframe = data_eltontraits, speciescolumn =
                                                  "Scientific")
  
  
  ##### 4. Keep only one row per species  #####
  
  
  # We remove from data_combine rows which did not find corresponding names in data_gbif
  if (length(which(is.na(data_eltontraits$species_gbif))) > 0) {
    data_eltontraits = data_eltontraits[-which(is.na(data_eltontraits$species_gbif)),]
  }
  
  
  
  # Removes data we are not interested in
  data_eltontraits = subset(
    data_eltontraits,
    select = -c(
      SpecID,
      PassNonPass,
      IOCOrder,
      BLFamilyLatin,
      BLFamilyEnglish,
      BLFamSequID,
      Taxo,
      Scientific,
      English,
      MSW3_ID,
      MSWFamilyLatin,
      ForStrat.Value,
      ForStrat.Certainty,
      ForStrat.Comment,
      Activity.Nocturnal,
      Activity.Crepuscular,
      Activity.Diurnal,
      Activity.Source,
      Activity.Certainty
    )
  )
  
  # Removes replicates rows
  data_eltontraits = distinct(data_eltontraits)
  
  
  
  # As we don't want to compute averages of values of different statistical status
  # (averages of unknown n, unique observations) have different weights, we will have to choose one data per species and per variable instead
  # The number of data for each species is very often 1 or 2. It is therefore risky to compute median as it would result in non weighted averages.
  # Therefore for each species, if there are several data for the same variable we keep the data coming from the reference having the most data
  
  
  list_sp = unique(data_eltontraits$species_gbif)
  if (length(which(is.na(list_sp))) > 0) {
    list_sp = list_sp[-which(is.na(list_sp))]
  }
  
  data_eltontraits_aggregated = NULL
  mat_na = 1 * !is.na(data_eltontraits) # Where are data
  col_choice = c(
    which(names(data_eltontraits) == "Diet.Inv"),
    which(names(data_eltontraits) == "Diet.Vend"),
    which(names(data_eltontraits) == "Diet.Vect"),
    which(names(data_eltontraits) == "Diet.Vfish"),
    which(names(data_eltontraits) == "Diet.Vunk"),
    which(names(data_eltontraits) == "Diet.Scav"),
    which(names(data_eltontraits) == "Diet.Fruit"),
    which(names(data_eltontraits) == "Diet.Nect"),
    which(names(data_eltontraits) == "Diet.Seed"),
    which(names(data_eltontraits) == "Diet.PlantO"),
    which(names(data_eltontraits) == "Diet.5Cat"),
    which(names(data_eltontraits) == "Diet.Source"),
    which(names(data_eltontraits) == "Diet.Certainty"),
    which(names(data_eltontraits) == "Diet.EnteredBy"),
    which(names(data_eltontraits) == "BodyMass.Value"),
    which(names(data_eltontraits) == "BodyMass.Source"),
    which(names(data_eltontraits) == "BodyMass.SpecLevel"),
    which(names(data_eltontraits) == "BodyMass.Comment")
  )
  
  row_data_nb = rep(NA, nrow(data_eltontraits))
  
  for (i in 1:length(list_sp)) {
    lines_species = which(data_eltontraits$species_gbif == list_sp[i]) # select the lines of the current species
    if (length(lines_species) == 1) {
      data_eltontraits_aggregated = bind_rows(data_eltontraits_aggregated, data_eltontraits[lines_species,])
    } # if there is only on row for the current species, we keep that row
    else {
      for (j in col_choice) {
        for (k in lines_species) {
          row_data_nb[k] = sum(mat_na[k, col_choice])
        }
        combined_row = data_eltontraits[lines_species[which(row_data_nb[lines_species] ==
                                                              max(row_data_nb[lines_species]))][1],]
      }
      data_eltontraits_aggregated = bind_rows(data_eltontraits_aggregated, combined_row)
    }
  }
  
  names(data_eltontraits_aggregated) = gsub("\\.", "_", names(data_eltontraits_aggregated))
  names(data_eltontraits_aggregated) = tolower(names(data_eltontraits_aggregated))
  
  return(data_eltontraits_aggregated)
  
}