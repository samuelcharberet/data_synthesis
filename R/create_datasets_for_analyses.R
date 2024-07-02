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
  data_selected_method <- data |>
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
      )
    )
  
  
  #### No cloaca (mammals) species averages with body mass and diet ####
  stock_data <- data_selected_method |>
    filter(component_data_type == "stock")
  
  faeces_stock_data <- stock_data |>
    filter(cloaca == 0)
  faeces_stock_data <- faeces_stock_data |>
    filter(sample_type == "feces" |
             sample_type == "faeces" &
             component_weight_type == "dw")
  # Selecting CNP in faeces stock data
  cnp_fsd <- faeces_stock_data |>
    filter(component_name == "C" |
             component_name == "N" | component_name == "P")
  
  # Keep only relevant rows
  
  cnp_fsd <- cnp_fsd |>
    dplyr::select(species_latin_name_gbif,
                  component_name,
                  component_mean,
                  body_mass,
                  diet)
  
  # Average over species
  
  cnp_fsd_species <- cnp_fsd |>
    group_by(species_latin_name_gbif, component_name) |>
    dplyr::summarise(
      avg_component_mean = mean(component_mean),
      n_obs = length(component_mean),
      body_mass = first(body_mass),
      diet = first(diet)
    )
  
  # Compute the ratios
  species = unique(cnp_fsd_species$species_latin_name_gbif)
  
  for (i in species) {
    crow = which(
      cnp_fsd_species$species_latin_name_gbif == i &
        cnp_fsd_species$component_name == "C"
    )
    nrow = which(
      cnp_fsd_species$species_latin_name_gbif == i &
        cnp_fsd_species$component_name == "N"
    )
    prow = which(
      cnp_fsd_species$species_latin_name_gbif == i &
        cnp_fsd_species$component_name == "P"
    )
    cn_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/N",
      avg_component_mean = ifelse(
        any(crow) &&
          any(nrow),
        log10(
          cnp_fsd_species$avg_component_mean[crow] / cnp_fsd_species$avg_component_mean[nrow]
        ),
        NA
      ),
      body_mass = cnp_fsd_species[which(cnp_fsd_species$species_latin_name_gbif == i)[1], "body_mass"],
      diet = cnp_fsd_species[which(cnp_fsd_species$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "C"), ]), nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                                                                                 cnp_fsd$component_name == "N"), ]))
    )
    cp_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/P",
      avg_component_mean = ifelse(
        any(crow) &&
          any(prow),
        log10(
          cnp_fsd_species$avg_component_mean[crow] / cnp_fsd_species$avg_component_mean[prow]
        ),
        NA
      ),
      body_mass = cnp_fsd_species[which(cnp_fsd_species$species_latin_name_gbif == i)[1], "body_mass"],
      diet = cnp_fsd_species[which(cnp_fsd_species$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "C"), ]), nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                                                                                 cnp_fsd$component_name == "P"), ]))
    )
    np_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "N/P",
      avg_component_mean = ifelse(
        any(nrow) &&
          any(prow),
        log10(
          cnp_fsd_species$avg_component_mean[nrow] / cnp_fsd_species$avg_component_mean[prow]
        ),
        NA
      ),
      body_mass = cnp_fsd_species[which(cnp_fsd_species$species_latin_name_gbif == i)[1], "body_mass"],
      diet = cnp_fsd_species[which(cnp_fsd_species$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "N"), ]), nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                                                                                 cnp_fsd$component_name == "P"), ]))
    )
    
    # Add Row using rbind()
    cnp_fsd_species = rbind(cnp_fsd_species, cn_row, cp_row, np_row)
  }
  
  # Log C, N and P contents (they are also ratios in nature)
  
  for (i in 1:nrow(cnp_fsd_species)) {
    if (cnp_fsd_species$component_name[i] %in% c("C", "N", "P")) {
      cnp_fsd_species$avg_component_mean[i] = log10(cnp_fsd_species$avg_component_mean[i] /
                                                      100)
    }
  }
  
  write.csv(cnp_fsd_species,
            here::here("1_data", "cnp_fsd_species.csv"),
            row.names = FALSE)
  
  
  #### Cloaca species averages with body mass and diet ####
  
  guano_stock_data <- stock_data |>
    filter(cloaca == 1)
  guano_stock_data <- guano_stock_data |>
    filter(sample_type == "frass" |
             sample_type == "guano" | component_weight_type == "dw")
  # Selecting CNP in guano stock data
  cnp_gsd <- guano_stock_data |>
    filter(component_name == "C" |
             component_name == "N" | component_name == "P")
  
  # Keep only relevant rows
  
  cnp_gsd <- cnp_gsd |>
    dplyr::select(species_latin_name_gbif,
                  component_name,
                  component_mean,
                  body_mass,
                  diet)
  
  # Average over species
  
  cnp_gsd_species <- cnp_gsd |>
    group_by(species_latin_name_gbif, component_name) |>
    dplyr::summarise(
      avg_component_mean = mean(component_mean),
      body_mass = first(body_mass),
      diet = first(diet)
    )
  
  # Compute the ratios
  species = unique(cnp_gsd_species$species_latin_name_gbif)
  
  for (i in species) {
    crow = which(
      cnp_gsd_species$species_latin_name_gbif == i &
        cnp_gsd_species$component_name == "C"
    )
    nrow = which(
      cnp_gsd_species$species_latin_name_gbif == i &
        cnp_gsd_species$component_name == "N"
    )
    prow = which(
      cnp_gsd_species$species_latin_name_gbif == i &
        cnp_gsd_species$component_name == "P"
    )
    cn_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/N",
      avg_component_mean = ifelse(
        any(crow) &&
          any(nrow),
        log10(
          cnp_gsd_species$avg_component_mean[crow] / cnp_gsd_species$avg_component_mean[nrow]
        ),
        NA
      ),
      body_mass = cnp_gsd_species[which(cnp_gsd_species$species_latin_name_gbif == i)[1], "body_mass"],
      diet = cnp_gsd_species[which(cnp_gsd_species$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                        cnp_gsd$component_name == "C"), ]), nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                                                                                 cnp_gsd$component_name == "N"), ]))
    )
    cp_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/P",
      avg_component_mean = ifelse(
        any(crow) &&
          any(prow),
        log10(
          cnp_gsd_species$avg_component_mean[crow] / cnp_gsd_species$avg_component_mean[prow]
        ),
        NA
      ),
      body_mass = cnp_gsd_species[which(cnp_gsd_species$species_latin_name_gbif == i)[1], "body_mass"],
      diet = cnp_gsd_species[which(cnp_gsd_species$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                        cnp_gsd$component_name == "C"), ]), nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                                                                                 cnp_gsd$component_name == "P"), ]))
    )
    np_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "N/P",
      avg_component_mean = ifelse(
        any(nrow) &&
          any(prow),
        log10(
          cnp_gsd_species$avg_component_mean[nrow] / cnp_gsd_species$avg_component_mean[prow]
        ),
        NA
      ),
      body_mass = cnp_gsd_species[which(cnp_gsd_species$species_latin_name_gbif == i)[1], "body_mass"],
      diet = cnp_gsd_species[which(cnp_gsd_species$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                        cnp_gsd$component_name == "N"), ]), nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                                                                                 cnp_gsd$component_name == "P"), ]))
    )
    
    # Add Row using rbind()
    cnp_gsd_species = rbind(cnp_gsd_species, cn_row, cp_row, np_row)
  }
  
  
  # Log C, N and P contents (they are also ratios in nature)
  
  for (i in 1:nrow(cnp_gsd_species)) {
    if (cnp_gsd_species$component_name[i] %in% c("C", "N", "P")) {
      cnp_gsd_species$avg_component_mean[i] = log10(cnp_gsd_species$avg_component_mean[i] /
                                                      100)
    }
  }
  
  ### Write data file ###
  
  write.csv(cnp_gsd_species,
            here::here("1_data", "cnp_gsd_species.csv"),
            row.names = FALSE)
  
  #### Mammalia species data with custom bodymass and ratios ####
  
  # Selecting CNP in faeces stock data
  cnp_fsd <- faeces_stock_data |>
    filter(component_name == "C" |
             component_name == "N" | component_name == "P") 
  
  for (i in 1:nrow(cnp_fsd)) {
    if (!is.na(cnp_fsd$bodymass_mean[i])){
      cnp_fsd$body_mass[i] = cnp_fsd$bodymass_mean[i]
    }
  }
  
  # Pivot the data to get C, N, P in separate columns
  df_pivot <- cnp_fsd %>%
    select(reference_ID,
           feces_ID,
           repetition_ID,
           component_name,
           component_mean) %>%
    pivot_wider(names_from = component_name, values_from = component_mean)
  
  # Calculate ratios
  df_ratios <- df_pivot %>%
    mutate(
      C_N = ifelse(!is.na(C) & !is.na(N), C / N, NA),
      C_P = ifelse(!is.na(C) & !is.na(P), C / P, NA),
      N_P = ifelse(!is.na(N) & !is.na(P), N / P, NA)
    )
  
  a = cnp_fsd %>%
    left_join(df_ratios, by = c("reference_ID", "feces_ID", "repetition_ID"))
  

}
