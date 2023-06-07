#' read_etwp
#'A function to extract data from the
# Ecological Traits of the World's Primates database (ETWP)
# Here we extract bodymass and diet data only
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_etwp = function(path) {
  setwd(path)
  
  ##### 1. Import ETWP databases #####
  
  etwp_bm = read.csv("./BodyMass_.csv")
  etwp_tg = read.csv("./TrophicGuild.csv")
  
  # clean column names
  
  names(etwp_bm) = c(
    "family",
    "genus",
    "common_name",
    "species",
    "species_itis",
    "body_mass",
    "body_mass_male",
    "body_mass_female",
    "refs1"
  )
  etwp_bm$body_mass_unit = "kg"
  
  names(etwp_tg) = c(
    "family",
    "genus",
    "common_name",
    "species",
    "species_itis",
    "trophic_guild",
    "refs",
    "fruit",
    "leaves",
    "flowers",
    "seeds",
    "animal",
    "gums",
    "nectar",
    "other",
    "region",
    "country",
    "data_type",
    "study_duration",
    "n_groups",
    "mean_individuals",
    "comments",
    "x"
  )
  
  etwp_tg$study_duration_unit = "month"
  
  ##### 2. Add data from the GBIF Backbone Taxonomy  #####
  
  # A function to download data from the GBIF Backbone Taxonomy and combine
  # it with a given data frame containing a column with species names
  
  etwp_bm = add_gbif_backbone_taxonomy(dataframe = etwp_bm, speciescolumn =
                                         "species_itis")
  etwp_tg = add_gbif_backbone_taxonomy(dataframe = etwp_tg, speciescolumn =
                                         "species_itis")
  
  
  ##### 3. Body mass data #####
  
  
  # We remove from etwp_bm rows which did not find corresponding names in data_gbif
  etwp_bm = etwp_bm[-which(is.na(etwp_bm$species_gbif)),]
  
  # Removes taxonomic data to only keep GBIF data and remove data unused in the NetSto project
  etwp_bm = subset(etwp_bm,
                   select = -c(family, genus, common_name, species, species_itis, refs1))
  
  # Removes replicates rows
  etwp_bm = distinct(etwp_bm)
  
  # Body mass is expressed in kilograms; values for this variable can represent reported individual values,
  # reported averages, or calculated averages (when a study included more than one body mass datum).
  # When available, we separately report the mean body mass of adult males and females.
  
  
  # As we don't want to compute averages of values of different statistical status
  # (averages of unknown n, unique observations) have different weights, we will have to choose one data per species instead
  # The number of data for each species is very often 1 or 2. It is therefore risky to compute median as it would result in averages.
  # For each species, if there are several data for the same variable
  # we keep the data coming from the reference having the most data
  
  list_sp_bm = unique(etwp_bm$species_gbif)
  
  if (length(which(is.na(list_sp_bm))) > 0) {
    list_sp_bm = list_sp_bm[-which(is.na(list_sp_bm))]
  }
  
  etwp_bm_aggregated = NULL
  mat_na = 1 * !is.na(etwp_bm) # Where are data
  col_bm = c(
    which(names(etwp_bm) == "body_mass"),
    which(names(etwp_bm) == "body_mass_male"),
    which(names(etwp_bm) == "body_mass_female")
  )
  row_data_nb = rep(NA, nrow(etwp_bm))
  
  for (i in 1:length(list_sp_bm)) {
    lines_species = which(etwp_bm$species_gbif == list_sp_bm[i]) # select the lines of the current species
    if (length(lines_species) == 1) {
      etwp_bm_aggregated = bind_rows(etwp_bm_aggregated, etwp_bm[lines_species,])
    } # if there is only on row for the current species, we keep that row
    else {
      combined_row = etwp_bm[lines_species[1],]
      
      for (j in col_bm) {
        if (sum(mat_na[lines_species, j]) == 0) {
          combined_row[1, j] = NA
        }
        else if (sum(mat_na[lines_species, j]) == 1) {
          combined_row[1, j] = etwp_bm[lines_species[which(mat_na[lines_species, j] ==
                                                             1)], j]
        }
        else if (sum(mat_na[lines_species, j]) > 1) {
          for (k in lines_species) {
            row_data_nb[k] = sum(mat_na[k, col_bm])
          }
          combined_row[1, j] = etwp_bm[lines_species[which(row_data_nb[lines_species] ==
                                                             max(row_data_nb[lines_species]))][1], j]
        }
      }
      etwp_bm_aggregated = bind_rows(etwp_bm_aggregated, combined_row[1,])
    }
  }
  
  ##### 4. Diet data #####
  
  # We remove from etwp_tg rows which did not find corresponding names in data_gbif
  etwp_tg = etwp_tg[-which(is.na(etwp_tg$species_gbif)),]
  
  # Removes phylacine and IUCN taxonomic data to only keep GBIF data and remove data not used in the NetSto project
  
  etwp_tg = subset(
    etwp_tg,
    select = -c(
      family,
      genus,
      common_name,
      species,
      species_itis,
      refs,
      region,
      country,
      study_duration,
      n_groups,
      mean_individuals,
      comments,
      x,
      study_duration_unit
    )
  )
  
  # Removes replicates rows
  etwp_tg = distinct(etwp_tg)
  
  # Metadata given by the authors:
  # frugivore (>60% of fruits/seeds in diet), folivore (>60% leaves in diet),
  # folivore-frugivore (diet comprised of both fruits/seeds and leaves in similar proportions),
  # omnivore (diet comprised of both plants and animals in similar proportions),
  # insectivore (>50% of arthropods in diet) and gummivore (diet dominated by plant exudates).
  # We include in the trophic guild data base the percentage of fruit, leaves, flowers, seeds, animal matter, nectar and other, when available.
  
  # The number of data for each species is very often 1 or 2. It is therefore risky to compute median as it would result in non weighted averages.
  # Therefore for each species, if there are several data for the same variable we keep the data coming from the reference having the most data
  
  list_sp_tg = unique(etwp_tg$species_gbif)
  if (length(which(is.na(list_sp_tg))) > 0) {
    list_sp_tg = list_sp_tg[-which(is.na(list_sp_tg))]
  }
  
  etwp_tg_aggregated = NULL
  etwp_tg[which(etwp_tg == "NI", arr.ind = T)] = NA
  mat_na = 1 * !is.na(etwp_tg) # Where are data
  col_tg = c(
    which(names(etwp_tg) == "trophic_guild"),
    which(names(etwp_tg) == "fruit"),
    which(names(etwp_tg) == "leaves"),
    which(names(etwp_tg) == "flowers"),
    which(names(etwp_tg) == "seeds"),
    which(names(etwp_tg) == "animal"),
    which(names(etwp_tg) == "gums"),
    which(names(etwp_tg) == "nectar"),
    which(names(etwp_tg) == "other")
  )
  
  row_data_nb = rep(NA, nrow(etwp_tg))
  
  for (i in 1:length(list_sp_tg)) {
    lines_species = which(etwp_tg$species_gbif == list_sp_tg[i]) # select the lines of the current species
    if (length(lines_species) == 1) {
      etwp_tg_aggregated = bind_rows(etwp_tg_aggregated, etwp_tg[lines_species,])
    } # if there is only on row for the current species, we keep that row
    else {
      combined_row = etwp_tg[lines_species[1],]
      if (length(unique(etwp_tg[lines_species, "data_type"])) == 1) {
        combined_row$data_type = unique(etwp_tg[lines_species, "data_type"])
      } # if all trophic information where retrieved using the same method for the current species, put it in the table otherwise put NA
      else {
        combined_row$data_type = NA
      }
      
      for (j in col_tg) {
        if (sum(mat_na[lines_species, j]) == 0) {
          combined_row[1, j] = NA
        }
        else if (sum(mat_na[lines_species, j]) == 1) {
          combined_row[1, j] = etwp_tg[lines_species[which(mat_na[lines_species, j] ==
                                                             1)], j]
        }
        else if (sum(mat_na[lines_species, j]) > 1) {
          for (k in lines_species) {
            row_data_nb[k] = sum(mat_na[k, col_tg])
          }
          combined_row[1, j] = etwp_tg[lines_species[which(row_data_nb[lines_species] ==
                                                             max(row_data_nb[lines_species]))][1], j]
        }
      } # We base our data choice for each variable on data completeness at the level of the row
      etwp_tg_aggregated = bind_rows(etwp_tg_aggregated, combined_row[1,])
    }
  }
  
  ##### 5. Merge bodymass and diet data #####
  
  etwp = merge(etwp_bm_aggregated, etwp_tg_aggregated, by = "species_gbif")
  etwp = subset(
    etwp,
    select = -c(
      canonical_name_gbif.y,
      rank_gbif.y,
      status_gbif.y,
      match_type_gbif.y,
      specieskey_gbif.y,
      phylum_gbif.y,
      class_gbif.y,
      order_gbif.y,
      family_gbif.y,
      genus_gbif.y
    )
  )
  
  names(etwp) = c(
    "species_gbif",
    "body_mass",
    "body_mass_male",
    "body_mass_female",
    "body_mass_unit",
    "canonical_name_gbif",
    "rank_gbif",
    "status_gbif",
    "match_type_gbif",
    "specieskey_gbif",
    "phylum_gbif",
    "class_gbif",
    "order_gbif",
    "family_gbif",
    "genus_gbif",
    "trophic_guild",
    "fruit",
    "leaves",
    "flowers",
    "seeds",
    "animal",
    "gums",
    "nectar",
    "other",
    "diet_unit"
  )
  
  etwp$diet_unit = "%"
  return(etwp)
  
}