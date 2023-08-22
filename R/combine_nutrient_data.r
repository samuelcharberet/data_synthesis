################################ NETSTO PROJECT ################################

#' combine_nutrient_data
#'
#' @param path
#'
#' @return a table resulting from the vertical merge of tables from the NetSto nutrients database
#' with added taxonomic data from the Global Biodiversity Information Facility
#' @export
#'
#' @examples
#' @authors Anne-Cecile vain, Lucie Serre, Jerome Mathieu, Samuel Charberet
#'
combine_nutrient_data = function(data_nl, data_np) {
  ######  3. Vertical concatenation of all tables  ######
  
  data_nutrients = plyr::rbind.fill (plyr::rbind.fill(data_nl), data_np)
  rm(data_nl, data_np)
  # We remove rows containing only NAs
  data_nutrients = data_nutrients[rowSums(is.na(data_nutrients)) != ncol(data_nutrients), ]
  
  ######  4. Homogenize the nomenclature of the species variables ####
  
  # of the species variables
  data_nutrients$species_latin_name_gbif = tolower(data_nutrients$species_latin_name_gbif)
  data_nutrients$species_latin_name_gbif = gsub(" ", "_", data_nutrients$species_latin_name_gbif)
  
  
  # of the component names
  data_nutrients[which(data_nutrients$component_name == "egestion_rate"),]$component_name = "egestion"
  data_nutrients[which(data_nutrients$component_name == "15N"),]$component_name =
    "d15N"
  data_nutrients[which(data_nutrients$component_name == "NH4+"),]$component_name =
    "NH4"
  data_nutrients[which(data_nutrients$component_name == "NO3-"),]$component_name =
    "NO3"
  data_nutrients[which(data_nutrients$component_name == "uric acid"),]$component_name = "uric_acid"
  data_nutrients[which(data_nutrients$component_name == "ingestion"),]$component_name = "intake"
  data_nutrients[which(data_nutrients$component_name == "C:N"),]$component_name = "C/N"
  data_nutrients[which(data_nutrients$component_name == "PO43-"),]$component_name = "PO4"
  
  # of the bodymass weight type
  data_nutrients[which(data_nutrients$bodymass_weight_type == "ww"),]$bodymass_weight_type = "fw"
  data_nutrients[which(data_nutrients$bodymass_weight_type == "WW"),]$bodymass_weight_type = "fw"
  
  # of the age error type
  data_nutrients[which(data_nutrients$age_error_type == "sd"),]$age_error_type = "standard_deviation"
  
  # of the sampling environment
  data_nutrients[which(data_nutrients$environment == "lab"),]$environment = "laboratory"
  
  # of the observation resolution
  data_nutrients[which(data_nutrients$observation_resolution == "intra_indiv"),]$observation_resolution = "intra_individual"
  data_nutrients[which(data_nutrients$observation_resolution == "inter_indiv"),]$observation_resolution = "inter_individual"
  
  # of the component error type
  data_nutrients[which(data_nutrients$component_error_type == "sd"),]$component_error_type = "standard_deviation"
  data_nutrients[which(data_nutrients$component_error_type == "SE"),]$component_error_type = "standard_error"
  
  # of the component error type
  data_nutrients[which(data_nutrients$component_unit == "mg/g"),]$component_unit = "g/kg"
  data_nutrients[which(data_nutrients$component_unit == "pourcentage"),]$component_unit = "percent"
  data_nutrients[which(data_nutrients$component_unit == "percentage"),]$component_unit = "percent"
  data_nutrients[which(data_nutrients$component_unit == "g/100g"),]$component_unit = "percent"
  data_nutrients[which(data_nutrients$component_unit == "mg/kg"),]$component_unit = "ppm"
  data_nutrients[which(data_nutrients$component_unit == "µg/g"),]$component_unit = "ppm"
  data_nutrients[which(data_nutrients$component_unit == "mg N/kg"),]$component_unit = "mgN/kg"
  data_nutrients[which(data_nutrients$component_unit == "g/indiv/day"),]$component_unit = "g/day/individual"
  data_nutrients[which(data_nutrients$component_unit == "parts_per_thousand"),]$component_unit = "g/kg"
  data_nutrients[which(data_nutrients$component_unit == "%"),]$component_unit = "percent"
  
  # of the component weight type
  data_nutrients[which(data_nutrients$component_weight_type == "ww"),]$component_weight_type = "fw"
  
  # of the component detail
  data_nutrients[which(data_nutrients$component_detail == "TOC"),]$component_weight_type = "organic"
  
  # of the component measure method
  
  data_nutrients[which(data_nutrients$component_measure_method == "CHN"),]$component_measure_method = "elemental_analyser"
  data_nutrients[which(data_nutrients$component_measure_method == "flow_injection_analysis"),]$component_measure_method = "autoanalyzer"
  data_nutrients[which(data_nutrients$component_measure_method == "colorimetry_autoanalyzer"),]$component_measure_method = "autoanalyzer"
  data_nutrients[which(data_nutrients$component_measure_method == "autoanalyser"),]$component_measure_method = "autoanalyzer"
  data_nutrients[which(data_nutrients$component_measure_method == "AAS"),]$component_measure_method = "atomic_absorption_spectroscopy"
  data_nutrients[which(data_nutrients$component_measure_method == "aas"),]$component_measure_method = "atomic_absorption_spectroscopy"
  data_nutrients[which(data_nutrients$component_measure_method == "atomic_absorbtion_spectroscopy"),]$component_measure_method = "atomic_absorption_spectroscopy"
  data_nutrients[which(data_nutrients$component_measure_method == "elemental_analyzer"),]$component_measure_method = "elemental_analyser"
  data_nutrients[which(data_nutrients$component_measure_method == "ICP-OES"),]$component_measure_method = "icp_oes"
  data_nutrients[which(data_nutrients$component_measure_method == "icp-oes"),]$component_measure_method = "icp_oes"
  data_nutrients[which(data_nutrients$component_measure_method == "micro-kjeldahl"),]$component_measure_method = "micro_kjeldahl"
  data_nutrients[which(data_nutrients$component_measure_method == "icp-ms"),]$component_measure_method = "icp_ms"
  data_nutrients[which(data_nutrients$component_measure_method == "segmented_flow_autoanalyzer"),]$component_measure_method = "autoanalyzer"
  data_nutrients[which(data_nutrients$component_measure_method == "icp-aes"),]$component_measure_method = "icp_oes"
  data_nutrients[which(data_nutrients$component_measure_method == "atomic_absorption_spectrometry"),]$component_measure_method = "atomic_absorption_spectroscopy"
  data_nutrients[which(
    data_nutrients$component_measure_method == "atomic_absorption_spectrophotometer"
  ),]$component_measure_method = "atomic_absorption_spectroscopy"
  data_nutrients[which(
    data_nutrients$component_measure_method == "flame-atomic_absorption_spectrophotometry"
  ),]$component_measure_method = "atomic_absorption_spectroscopy"
  data_nutrients[which(data_nutrients$component_measure_method == "Kjeldahl"),]$component_measure_method = "kjeldahl"
  data_nutrients[which(data_nutrients$component_measure_method == "phosphomolybdenum_blue_method"),]$component_measure_method = "molybdenum_blue"
  data_nutrients[which(
    data_nutrients$component_measure_method == "atomic-absorption spectrophotometry"
  ),]$component_measure_method = "atomic_absorption_spectroscopy"
  data_nutrients[which(data_nutrients$component_measure_method == "flame_photometry"),]$component_measure_method = "atomic_absorption_spectroscopy"
  data_nutrients[which(data_nutrients$component_measure_method == "molybdate_method"),]$component_measure_method = "molybdenum_blue"
  data_nutrients[which(data_nutrients$component_measure_method == "spectrophotometer"),]$component_measure_method = "spectrophotometry"
  data_nutrients[which(data_nutrients$component_measure_method == "molybdenum_blue_method"),]$component_measure_method = "molybdenum_blue"
  data_nutrients[which(data_nutrients$component_measure_method == "macro-Kjeldahl"),]$component_measure_method = "macro_kjeldahl"
  data_nutrients[which(data_nutrients$component_measure_method == "ammonium_molybdate"),]$component_measure_method = "molybdenum_blue"
  data_nutrients[which(data_nutrients$component_measure_method == "continuous_flow_analyzer"),]$component_measure_method = "autoanalyzer"
  data_nutrients[which(data_nutrients$component_measure_method == "micro_kjeldahl"),]$component_measure_method = "micro_kjeldahl"
  data_nutrients[which(data_nutrients$component_measure_method == "ascorbic_acid_method"),]$component_measure_method = "micro-ascorbic_acid"
  data_nutrients[which(data_nutrients$component_measure_method == "walkley-black"),]$component_measure_method = "walkley_black"
  data_nutrients[which(data_nutrients$component_measure_method == "khjeldal_AOAC-984.13"),]$component_measure_method = "kjeldahl"
  
  # of the drying time unit
  data_nutrients[which(data_nutrients$drying_time_unit == "h"),]$drying_temp_unit = "hour"
  
  
  # of the drying temperature unit
  data_nutrients[which(data_nutrients$drying_temp_unit == "°C"),]$drying_temp_unit = "c"
  data_nutrients[which(data_nutrients$drying_temp_unit == "C"),]$drying_temp_unit = "c"
  
  # of the grinding fineness unit
  data_nutrients[which(data_nutrients$grinding_fineness_unit == "microns"),]$grinding_fineness_unit = "µm"
  
  
  ######  4. Homogenize the values depending on the units ####
  
  # Carbon, nitrogen, phosphorus, potassium, magnesium, sulfur in %
  
  data_nutrients = data_nutrients[is.na(data_nutrients$component_mean) ==
                                    F,]
  
  columns_to_transform = c("component_mean",
                           "component_min",
                           "component_max",
                           "component_value_error")
  
  percent_rows = which(
    data_nutrients$component_name == "C" |
      data_nutrients$component_name == "N" |
      data_nutrients$component_name == "P" |
      data_nutrients$component_name == "O" |
      data_nutrients$component_name == "Mg" |
      data_nutrients$component_name == "S" |
      data_nutrients$component_name == "K"
  )
  
  molar_masses = c(
    "C" = 12.011,
    "N" = 14.007,
    "P" = 30.974,
    "O" = 15.999,
    "K" = 39.098,
    "Mg" = 24.305,
    "S" = 32.065
  )
  
  
  for (i in percent_rows) {
    unit = data_nutrients$component_unit[i]
    if (is.na(unit)) {
      
    }
    else if (unit == "g/kg") {
      data_nutrients[i, columns_to_transform] = data_nutrients[i, columns_to_transform] / 10
      data_nutrients$component_unit[i] = "percent"
    } else if (unit == "ppm") {
      data_nutrients[i, columns_to_transform] = data_nutrients[i, columns_to_transform] / 10000
      data_nutrients$component_unit[i] = "percent"
    }
    else if (unit == "µmol/g") {
      data_nutrients[i, columns_to_transform] = data_nutrients[i, columns_to_transform] * molar_masses[data_nutrients$component_name[i]] * 10 ^
        -4
      data_nutrients$component_unit[i] = "percent"
    }
    else if (unit == "mg/100g") {
      data_nutrients[i, columns_to_transform] = data_nutrients[i, columns_to_transform] / 1000
      data_nutrients$component_unit[i] = "percent"
    }
  }
  
  
  # All the other elements in ppm
  
  ppm_rows = which(
    data_nutrients$component_name == "Ca" |
      data_nutrients$component_name == "Na" |
      data_nutrients$component_name == "Fe" |
      data_nutrients$component_name == "Si" |
      data_nutrients$component_name == "Al" |
      data_nutrients$component_name == "Cl" |
      data_nutrients$component_name == "Ti" |
      data_nutrients$component_name == "Zn" |
      data_nutrients$component_name == "Mn" |
      data_nutrients$component_name == "Mo" |
      data_nutrients$component_name == "Cu" |
      data_nutrients$component_name == "B" |
      data_nutrients$component_name == "Cr" |
      data_nutrients$component_name == "Pb" |
      data_nutrients$component_name == "As" |
      data_nutrients$component_name == "Cd" |
      data_nutrients$component_name == "Ni" |
      data_nutrients$component_name == "V" |
      data_nutrients$component_name == "Hg" |
      data_nutrients$component_name == "Co" |
      data_nutrients$component_name == "Se" |
      data_nutrients$component_name == "Sr" |
      data_nutrients$component_name == "Ba"
  )
  
  molar_masses = c(
    "Ca" = 40.078,
    "Na" = 22.990,
    "Fe" = 55.845,
    "Si" = 28.086,
    "Al" = 26.982,
    "Cl" = 35.453,
    "Ti" = 47.867,
    "Zn" = 65.409,
    "Mn" = 54.938,
    "Mo" = 95.95,
    "Cu" = 63.546,
    "B" = 10.811,
    "Cr" = 51.996,
    "Pb" = 207.2,
    "As" = 74.921,
    "Cd" = 112.411,
    "Ni" = 58.693,
    "V" = 50.942,
    "Hg" = 200.59,
    "Co" = 58.933,
    "Se" = 78.96,
    "Sr" = 87.62,
    "Ba" = 137.327
  )
  
  for (i in ppm_rows) {
    unit = data_nutrients$component_unit[i]
    if (is.na(unit)) {
      
    }
    else if (unit == "g/kg") {
      data_nutrients[i, columns_to_transform] = data_nutrients[i, columns_to_transform] * 1000
      data_nutrients$component_unit[i] = "ppm"
    } else if (unit == "percent") {
      data_nutrients[i, columns_to_transform] = data_nutrients[i, columns_to_transform] * 10000
      data_nutrients$component_unit[i] = "ppm"
    }
    else if (unit == "µg/kg") {
      data_nutrients[i, columns_to_transform] = data_nutrients[i, columns_to_transform] / 1000
      data_nutrients$component_unit[i] = "ppm"
    }
    else if (unit == "µmol/g") {
      data_nutrients[i, columns_to_transform] = data_nutrients[i, columns_to_transform] * molar_masses[data_nutrients$component_name[i]]
      data_nutrients$component_unit[i] = "ppm"
    }
  }
  
  
  
  ##### 6. Finding where there are GBIF_ID given to a taxonomic level higher than species ####
  
  gbif_ids = unique(data_nutrients$gbif_id)
  
  # We download the taxonomic data every three months at least
  
  if (file.exists(here::here("1_data", "4_data_taxonomy", "list_rank.RData")) ==
      T) {
    if (file.info(here::here("1_data", "4_data_taxonomy", "list_rank.RData"))$mtime - Sys.time() < as.difftime(12, units = "weeks")) {
      list_data_gbif = readRDS(here::here("1_data", "4_data_taxonomy", "list_data_gbif.RData"))
    }
    list_rank = readRDS(here::here("1_data", "4_data_taxonomy", "list_rank.RData"))
    update = F
  } else {
    list_rank = id2name(gbif_ids, db = "gbif")
    saveRDS(list_rank,
            file = here::here("1_data", "4_data_taxonomy", "list_rank.RData"))
    update = T
  }
  rm(gbif_ids)
  
  rank_notspecies = which(sapply(list_rank, function(e)
    ! is.element('species', e)))
  not_species_id = as.integer(names(list_rank[rank_notspecies]))
  rm(list_rank, rank_notspecies)
  if (length(which(is.na(not_species_id))) > 0) {
    not_species_id = not_species_id[-which(is.na(not_species_id))]
  }
  
  ##### 7. Remove data with taxonomic level higher than species ####
  
  data_nutrients <-
    data_nutrients[!(data_nutrients$gbif_id %in% not_species_id),]
  rm(not_species_id)
  
  
  ##### 8. Create a table with taxonomic data for each species  ####
  
  # creates a character object with n element, n the number of unique species name
  list_sp <-
    unique(data_nutrients$species_latin_name_gbif) # There are x different species names in the raw nutrients database
  # Get the GBIF taxonomic data for each species from their taxonomic names
  # get_gbifid_ is a function to search GBIF's taxonomy
  # if an exact match is found, the ID for that match is returned
  # If there isn't an exact match, the options are returned in a dataframe, and we have to pick up one.
  # The match can be either exacvt or fuzzy, and the name can either be synonym or accepted
  # Here we create a list of data frames containing one or more match proposals
  
  # We download the taxonomic data every three months at least
  if (file.exists(here::here("1_data", "4_data_taxonomy", "list_data_gbif.RData")) ==
      T) {
    if (file.info(here::here("1_data", "4_data_taxonomy", "list_data_gbif.RData"))$mtime - Sys.time() < as.difftime(12, units = "weeks")) {
      list_data_gbif = readRDS(here::here("1_data", "4_data_taxonomy", "list_data_gbif.RData"))
    }
  } else {
    list_data_gbif = get_gbifid_(gsub("_", " ", list_sp))
    saveRDS(
      list_data_gbif,
      file = here::here("1_data", "4_data_taxonomy", "list_data_gbif.RData")
    )
  }
  
  # Create a table with taxonomic data for each species
  # This table is supposed to have the same number of rows as the number of unique species names in data_nutrients
  # We select in priority exact matches and accepted species names, then synonym species names.
  # If there are no exact but only fuzzy matches ,we select in priority accepted species names and then synonym species names
  
  data_gbif <- NULL
  
  for (i in 1:length(list_data_gbif)) {
    if (nrow(list_data_gbif[[i]]) == 1) {
      data_gbif <- bind_rows(data_gbif, list_data_gbif[[i]])
    } else if (nrow(list_data_gbif[[i]]) == 0) {
      ligne_na = rep(NA, length.out = length(list_data_gbif[[1]]))
      names(ligne_na) = names(list_data_gbif[[1]])
      
      data_gbif = bind_rows(data_gbif, ligne_na)
    } else {
      accepted_rows = which(list_data_gbif[[i]]$status == "ACCEPTED")
      synonym_rows = which(list_data_gbif[[i]]$status == "SYNONYM")
      exact_rows = which(list_data_gbif[[i]]$matchtype == "EXACT")
      fuzzy_rows = which(list_data_gbif[[i]]$matchtype == "FUZZY")
      if (length(intersect(accepted_rows, exact_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence)[1])
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
      else if (length(intersect(synonym_rows, exact_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence)[1])
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
      else if (length(intersect(accepted_rows, fuzzy_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence)[1])
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
      else if (length(intersect(synonym_rows, fuzzy_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence)[1])
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
    }
  }
  rm(
    list_data_gbif,
    accepted_rows,
    exact_rows,
    fuzzy_rows,
    synonym_rows,
    ligne_na,
    max_conf
  )
  
  factor_columns = c("rank", "status", "matchtype")
  data_gbif[factor_columns] = lapply(data_gbif[factor_columns], factor)
  rm(factor_columns)
  
  
  ##### 9. Finding the subspecies if there are some ####
  
  subspecies = data_gbif[which(data_gbif$rank == "subspecies"), "canonicalname"] # find subspecies canonical names
  # If subspecies is empty, then it means that there are no subspecies in the dataset
  subspecies = as.character(subspecies)
  subspecies = tolower(subspecies)
  subspecies =  gsub(" ", "_", subspecies)
  
  if (length(subspecies) > 1) {
    warning("The database nutrients contains subspecies names instead of species binomial names")
    warning("The following tables contain subspecies ID")
    print(unique(data_nutrients[which(data_nutrients$species_latin_name_gbif == subspecies),]$reference_ID))
    warning("The following subspecies are named")
    warning(
      "Open the table associated with these references ID to put back the species names instead of subspecies"
    )
    print(subspecies)
  }
  rm(subspecies)
  
  
  ##### 10. Check the status of the scientific name  ####
  
  
  synonym = data_gbif[which(data_gbif$status == "SYNONYM"),]$canonicalname # find synonym species canonical names
  # If synonym is empty, then it means that there are no synonym species in the dataset
  synonym = as.character(synonym)
  synonym = tolower(synonym)
  synonym =  gsub(" ", "_", synonym)
  
  if (length(synonym) > 1) {
    warning("The database nutrients contains incorrect names.")
    warning("The following tables contain incorrect ID")
    print(unique(data_nutrients[which(data_nutrients$species_latin_name_gbif == synonym),]$reference_ID))
    warning("The following incorrect names are given")
    warning("Open the table associated with these references ID to put back the right species names")
    print(synonym)
  }
  rm(synonym)
  
  ##### 11. Check the match types ####
  
  fuzzy = data_gbif[which(data_gbif$matchtype == "FUZZY"),]$canonicalname # find fuzzy matches canonical names
  # If fuzzy is empty, then it means that there are no synonym species in the dataset
  fuzzy = as.character(fuzzy)
  fuzzy = tolower(fuzzy)
  fuzzy =  gsub(" ", "_", fuzzy)
  
  if (length(fuzzy) > 1) {
    warning("The database nutrients contains fuzzy names.")
    warning("The following tables contain fuzzy names")
    
    print(unique(data_nutrients[which(data_nutrients$species_latin_name_gbif == fuzzy),]$reference_ID)) # find references
    warning("The following fuzzy names are given")
    warning("Open the table associated with these references ID to put back the right species names")
    print(fuzzy)
  }
  rm(fuzzy)
  
  character_columns = c("rank", "status", "matchtype")
  data_gbif[character_columns] = lapply(data_gbif[character_columns], as.character)
  rm(character_columns)
  
  #### 12. Combine the GBIF data with the nutrients data  ####
  
  # select relevant data from the GBIF data
  useful_data_gbif =  data_gbif[c(
    "canonicalname",
    "rank",
    "status",
    "matchtype",
    "species",
    "specieskey",
    "phylum",
    "class",
    "order",
    "family",
    "genus"
  )]
  
  useful_data_gbif$species = tolower(useful_data_gbif$species)
  useful_data_gbif$species = gsub(" ", "_", useful_data_gbif$species)
  
  colnames(useful_data_gbif) <-
    c(
      "canonical_name",
      "rank",
      "status",
      "match_type",
      "species",
      "specieskey",
      "phylum",
      "class",
      "order",
      "family",
      "genus"
    )
  
  # Add columns to the data_nutrients table
  data_nutrients[, c(
    "canonical_name",
    "rank",
    "status",
    "match_type",
    "species",
    "phylum",
    "class",
    "order",
    "family",
    "genus"
  )] = NA
  
  # A for loop to combine the GBIF data with the nutrients data
  
  for (i in 1:nrow(data_nutrients)) {
    if (!is.na(data_nutrients$species_latin_name_gbif[i])) {
      data_nutrients[i , c(
        "canonical_name",
        "rank",
        "status",
        "match_type",
        "species",
        "gbif_id",
        "phylum",
        "class",
        "order",
        "family",
        "genus"
      )] =
        useful_data_gbif[which(useful_data_gbif$species == data_nutrients$species_latin_name_gbif[i]),
                         c(
                           "canonical_name",
                           "rank",
                           "status",
                           "match_type",
                           "species",
                           "specieskey",
                           "phylum",
                           "class",
                           "order",
                           "family",
                           "genus"
                         )]
    }
  }
  
  # We  generate a blank template table for traits so that it can be filled by hand
  
  species = na.omit(unique(data_nutrients$species))
  n_species = length(species)
  data_traits_blank = data.frame(
    species_names = species,
    body_mass = NA,
    diet = NA,
    source_body_mass = NA,
    comments = NA
  )
  write.csv(
    data_traits_blank,
    here::here("1_data",
               "3_data_traits",
               "data_traits_blank.csv"),
    row.names = FALSE
  )
  
  
  selected_data <- data_nutrients$class
  
  grouped_data <- unique(selected_data)
  
  # We write a class taxize object
  
  if (update == T) {
    taxize_classes <-
      classification(grouped_data, db = "gbif")
    saveRDS(
      taxize_classes,
      file = here::here("1_data",
                        "4_data_taxonomy",
                        "taxize_classes.RData")
    )
  }
  
  ######  14. Write a data file ######
  
  write.csv(
    data_nutrients,
    here::here(
      "1_data",
      "1_data_nutrients",
      "3_data_nutrients_combined",
      "data_nutrients_combined.csv"
    ),
    row.names = FALSE
  )
  
  data_nutrients
}
