################################ NETSTO PROJECT ################################

#' create_datasets_for_analyses
#'
#' @param combine_nutrients_traits
#'
#' @return tables resulting from the selection processes of the full dataset
#'
#' @examples
#' @authors Samuel Charberet
#'
#'


create_datasets_for_analyses = function(data) {
  # Selecting data based on the method
  data_selected <- data |>
    filter(
      component_measure_method %in% c(
        "elemental_analyser",
        "atomic_absorption_spectroscopy",
        "icp_oes",
        "micro_kjeldahl",
        "icp_ms",
        "kjeldahl",
        "macro_kjeldahl",
        "atomic_fluorescence_spectrometry",
        "icp"
      ),
      component_name == "C" |
        component_name == "N" | component_name == "P",
      component_data_type == "stock",
      component_weight_type == "dw"
    )
  
  
  # The function to create species-level datasets #####
  
  create_species_datasets = function(data) {
    # Keep only relevant rows
    data <- data |>
      dplyr::select(species_latin_name_gbif,
                    component_name,
                    component_mean,
                    body_mass,
                    diet)
    
    # Average over species
    
    data_species <- data |>
      group_by(species_latin_name_gbif, component_name) |>
      dplyr::summarise(
        avg_component_mean = mean(component_mean),
        n_obs = length(component_mean),
        body_mass = first(body_mass),
        diet = first(diet)
      )
    
    # Compute the ratios
    species = unique(data_species$species_latin_name_gbif)
    
    for (i in species) {
      crow = which(data_species$species_latin_name_gbif == i &
                     data_species$component_name == "C")
      nrow = which(data_species$species_latin_name_gbif == i &
                     data_species$component_name == "N")
      prow = which(data_species$species_latin_name_gbif == i &
                     data_species$component_name == "P")
      cn_row = data.frame(
        species_latin_name_gbif = i,
        component_name = "C/N",
        avg_component_mean = ifelse(
          any(crow) &&
            any(nrow),
          log10(
            data_species$avg_component_mean[crow] / data_species$avg_component_mean[nrow]
          ),
          NA
        ),
        body_mass = data_species[which(data_species$species_latin_name_gbif == i)[1], "body_mass"],
        diet = data_species[which(data_species$species_latin_name_gbif == i)[1], "diet"],
        n_obs = mean(nrow(data[which(data$species_latin_name_gbif == i &
                                       data$component_name == "C"), ]), nrow(data[which(data$species_latin_name_gbif == i &
                                                                                          data$component_name == "N"), ]))
      )
      cp_row = data.frame(
        species_latin_name_gbif = i,
        component_name = "C/P",
        avg_component_mean = ifelse(
          any(crow) &&
            any(prow),
          log10(
            data_species$avg_component_mean[crow] / data_species$avg_component_mean[prow]
          ),
          NA
        ),
        body_mass = data_species[which(data_species$species_latin_name_gbif == i)[1], "body_mass"],
        diet = data_species[which(data_species$species_latin_name_gbif == i)[1], "diet"],
        n_obs = mean(nrow(data[which(data$species_latin_name_gbif == i &
                                       data$component_name == "C"), ]), nrow(data[which(data$species_latin_name_gbif == i &
                                                                                          data$component_name == "P"), ]))
      )
      np_row = data.frame(
        species_latin_name_gbif = i,
        component_name = "N/P",
        avg_component_mean = ifelse(
          any(nrow) &&
            any(prow),
          log10(
            data_species$avg_component_mean[nrow] / data_species$avg_component_mean[prow]
          ),
          NA
        ),
        body_mass = data_species[which(data_species$species_latin_name_gbif == i)[1], "body_mass"],
        diet = data_species[which(data_species$species_latin_name_gbif == i)[1], "diet"],
        n_obs = mean(nrow(data[which(data$species_latin_name_gbif == i &
                                       data$component_name == "N"), ]), nrow(data[which(data$species_latin_name_gbif == i &
                                                                                          data$component_name == "P"), ]))
      )
      
      # Add Row using rbind()
      data_species = rbind(data_species, cn_row, cp_row, np_row)
    }
    
    # Log C, N and P contents (they are also ratios in nature)
    
    for (i in 1:nrow(data_species)) {
      if (data_species$component_name[i] %in% c("C", "N", "P")) {
        data_species$avg_component_mean[i] = log10(data_species$avg_component_mean[i] /
                                                     100)
      }
    }
    return(data_species)
  }
  
  
  # Create a mammals dataset at the species level ####
  
  data_mammals <- data_selected |>
    filter(class == "Mammalia", sample_type == "feces")
  
  data_mammals_species = create_species_datasets(data_mammals)
  
  write.csv(
    data_mammals_species,
    here::here("1_data", "data_mammals_species.csv"),
    row.names = FALSE
  )
  
  # Create a Sauropsids dataset at the species level ####
  
  data_sauropsids <- data_selected |>
    filter(
      class == "Aves" | class == "Squamata" | class == "Testudines",
      sample_type == "guano" | sample_type == "feces"
    )
  
  data_sauropsids_species = create_species_datasets(data_sauropsids)
  
  write.csv(
    data_sauropsids_species,
    here::here("1_data", "data_sauropsids_species.csv"),
    row.names = FALSE
  )
  
  # Create a arthropods dataset at the species level ####
  
  data_arthropods <- data_selected |>
    filter(
      class == "Insecta" |
        class == "Diplopoda" |
        class == "Arachnida" |
        class == "Malacostraca" | class == "Collembola",
      sample_type == "frass" | sample_type == "feces"
    )
  
  data_arthropods_species = create_species_datasets(data_arthropods)
  
  write.csv(
    data_arthropods_species,
    here::here("1_data", "data_arthropods_species.csv"),
    row.names = FALSE
  )
  
  
  # The function to create observation-level datasets #####
  
  create_observations_datasets = function(data) {
    # Attributing a custom body mass
    # Either the known faeces producer average bodymass, or the average species body mass from the trait database
    for (i in 1:nrow(data)) {
      if (!is.na(data$bodymass_mean[i])) {
        data$body_mass[i] = data$bodymass_mean[i]
      }
    }
    
    # We give each row unique sample ID
    
    for (i in 1:nrow(data)) {
      if (!is.na(data$repetition_ID[i])) {
        data$sample_ID[i] = as.character(interaction(
          data$reference_ID[i],
          data$feces_ID[i],
          data$repetition_ID[i]
        ))
      } else {
        data$sample_ID[i] = as.character(interaction(data$reference_ID[i], data$feces_ID[i]))
      }
    }
    
    # We compute CNP ratios
    sample_IDs = unique(data$sample_ID[!is.na(data$sample_ID)])
    
    column_ratios = c(
      "location_in_reference",
      "observation_ID",
      grep("component", colnames(data), value = TRUE),
      "freezing",
      "autoclaving",
      "drying",
      "oven",
      "drying_time",
      "drying_time_unit",
      "drying_temp",
      "drying_temp_unit",
      "grinding",
      "grinding_fineness",
      "grinding_fineness_unit",
      "comments"
    )
    
    selected_columns = which(names(data) %in% column_ratios)
    couples = list(c("C", "N"), c("C", "P"), c("N", "P"))
    
    for (i in sample_IDs) {
      sample_rows = which(data$sample_ID == i)
      reference_rows = which(data$reference_ID == data$reference_ID[sample_rows[1]])
      elements = unique(data[sample_rows, ]$component_name)
      
      for (j in couples) {
        row_n = which(data$sample_ID == i &
                        data$component_name == j[1])
        
        row_d = which(data$sample_ID == i &
                        data$component_name == j[2])
        
        if (all(j %in% elements)) {
          # We create a new row
          data[nrow(data) + 1, -selected_columns] = data[sample_rows[1], -selected_columns]
          
          # The component mean is computed
          component_mean = data$component_mean[row_n] / data$component_mean[row_d]
          data[nrow(data), selected_columns] = list(
            NA,
            max(as.numeric(data$observation_ID[reference_rows])) + 1,
            paste(j[1], "/", j[2], sep = ""),
            component_mean,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            "dw",
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA
          )
        }
      }
    }
  }
  
  # Create a mammals dataset at the observation level ####
  
  data_mammals_observations = create_observations_datasets(data_mammals)
  write.csv(
    data_mammals_observations,
    here::here("1_data", "data_mammals_observations.csv"),
    row.names = FALSE
  )
  
  # Create a sauropsids dataset at the observation level ####
  
  data_sauropsids_observations = create_observations_datasets(data_sauropsids)
  write.csv(
    data_sauropsids_observations,
    here::here("1_data", "data_sauropsids_observations.csv"),
    row.names = FALSE
  )
  
  # Create an arthropod dataset at the observation level ####
  
  data_arthropods_observations = create_observations_datasets(data_arthropods)
  write.csv(
    data_arthropods_observations,
    here::here("1_data", "data_arthropods_observations.csv"),
    row.names = FALSE
  )
  
}
