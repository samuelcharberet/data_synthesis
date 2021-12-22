#' data_andrieux
# A function to extract data from the
# Andrieux 2020 Body stoichiometry of heterotrophs: assessing drivers of interspecific variations in elemental composition
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_andrieux = function(path) {
  setwd(path)
  ##### 1. Imports the Andrieux 2020 database #####
  
  data_andrieux = read.csv("Global_heterotroph_stoichio_v5.csv",
                           sep = ";",
                           dec = ',')
  
  colnames(data_andrieux)[1] = "Credit"
  ##### 2. Only keep terrestrial species #####
  
  data_andrieux = data_andrieux[which(data_andrieux$Habitat == "Terrestrial"),]
  length(unique(data_andrieux$Species_binomial))
  ##### 3. Remove body parts data  #####
  
  data_andrieux = data_andrieux[-which(data_andrieux$Stoichio_analysis == "Body part"),]
  
  
  ##### 4. Add data from the GBIF Backbone Taxonomy #####
  
  data_andrieux = add_gbif_backbone_taxonomy(dataframe = data_andrieux, speciescolumn =
                                               "Species_binomial")
  
  
  ##### 5. Keep only one row per species #####
  
  # We remove from data_combine rows which did not find corresponding names in data_gbif
  data_andrieux = data_andrieux[-which(is.na(data_andrieux$species_gbif)), ]
  
  # Removes Open Tree taxonomy  data to only keep GBIF taxonomic data and remove data unused in the NetSto project
  data_andrieux = subset(
    data_andrieux,
    select = -c(
      Diet_full,
      Class,
      Subclass,
      Superorder,
      Order,
      Suborder,
      Family,
      Genus,
      Species,
      Species_binomial,
      Group,
      Functional_group,
      Habitat,
      Mass_log10,
      Staff
    )
  )
  
  
  # As we don't want to compute averages of values of different statistical status
  # (averages of unknown n, unique observations) have different weights, we will have to choose one data per species and per variable instead
  # For each species, if there are several data for the same variable
  
  # And if all the data for the choice category are the same for all rows of a species, we keep it, otherwise we fill the cell with NA
  
  list_sp = unique(data_andrieux$species_gbif)
  
  
  if (length(which(is.na(list_sp))) > 0) {
    list_sp = list_sp[-which(is.na(list_sp))]
  }
  
  data_andrieux_aggregated = NULL
  mat_na = 1 * !is.na(data_andrieux) # Where are data
  
  col_choice = names(data_andrieux)[c(
    which(names(data_andrieux) == c("Credit")),
    which(names(data_andrieux) == c("Reference")),
    which(names(data_andrieux) == c("doi")),
    which(names(data_andrieux) == c("Organism_source")),
    which(names(data_andrieux) == c("Stoichio_analysis")),
    which(names(data_andrieux) == c("Nitrogen_calc_from_prot"))
  )]
  
  col_cont =   names(data_andrieux)[c(
    which(names(data_andrieux) == c("Mass_dry_full")),
    which(names(data_andrieux) == c("C_mean")),
    which(names(data_andrieux) == c("N_mean")),
    which(names(data_andrieux) == c("P_mean")),
    which(names(data_andrieux) == c("CN_ratio")),
    which(names(data_andrieux) == c("CP_ratio")),
    which(names(data_andrieux) == c("NP_ratio"))
  )]
  
  
  row_data_nb = rep(NA, nrow(data_andrieux))
  
  for (i in 1:length(list_sp)) {
    lines_species = which(data_andrieux$species_gbif == list_sp[i])# select the lines of the current species
    if (length(lines_species) == 1) {
      data_andrieux_aggregated = bind_rows(data_andrieux_aggregated, data_andrieux[lines_species, ])
    } # if there is only on row for the current species, we keep that row
    else {
      combined_row = data_andrieux[lines_species[1], ]
      
      for (j in col_cont) {
        if (sum(mat_na[lines_species, j]) == 0) {
          combined_row[1, j] = NA
        }
        else if (sum(mat_na[lines_species, j]) == 1) {
          combined_row[1, j] = data_andrieux[lines_species[which(mat_na[lines_species, j] ==
                                                                   1)], j]
        }
        else if (sum(mat_na[lines_species, j]) > 1) {
          combined_row[1, j] = median(data_andrieux[lines_species, j], na.rm = T)
        }
      }
      
      for (k in col_choice) {
        if (length(unique(data_andrieux[lines_species, k])) == 1) {
          combined_row[1, k] = data_andrieux[lines_species[1], k]
        }
        else {
          combined_row[1, k] = NA
        }
      }
      data_andrieux_aggregated = bind_rows(data_andrieux_aggregated, combined_row[1, ])
    }
  }
  
  names(data_andrieux_aggregated) = tolower(names(data_andrieux_aggregated))
  
  return(data_andrieux_aggregated)
  
}