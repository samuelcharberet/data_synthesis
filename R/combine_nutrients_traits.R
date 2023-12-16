################################ NETSTO PROJECT ################################

#' add_traits
#'
#' @param combine_nutrients_traits
#'
#' @return a table resulting from the merge of traits data with the data_nutrients table
#'
#' @examples
#' @authors Samuel Charberet
#'
#'
combine_nutrients_traits = function(data_n, data_t) {
  # We keep species_name, body mass and diet to add the the data_nutrients dataframe
  
  data_traits = data_t
  data_nutrients = data_n
  data_traits = data_traits[, c("species_names", "body_mass", "diet")]
  
  body_mass = NA
  diet = NA
  data = cbind(data_nutrients, body_mass, diet)
  species = na.omit(unique(data_nutrients$species))
  n_species = length(species)
  
  for (i in 1:n_species) {
    row_species = which(data$species == species[i])
    species_body_mass = data_traits[i, ]$body_mass
    species_diet = data_traits[i, ]$diet
    data[row_species, ]$body_mass = species_body_mass
    data[row_species, ]$diet = species_diet
  }
  
  ###### Structuration of the table ######
  
  
  # Add cloaca
  
  data$cloaca = NA
  
  for (i in 1:nrow(data)) {
    if (is.na(data$class[i])) {
      
    }
    else if (data$class[i] == "Mammalia") {
      data$cloaca[i] = 0
    } else {
      data$cloaca[i] = 1
    }
  }
  

  write.csv(data,
            here::here("1_data",
                       "data.csv"),
            row.names = FALSE)
  
  
  
  #### No cloaca (mammals) species averages with body mass and diet ####
  
  # Re-order the diet factor
  
  data$diet = str_to_title(data$diet)
  data$diet = factor(data$diet,
                     levels = c('Herbivore', 'Omnivore', 'Carnivore', 'Detritivore'))
  
  stock_data <- data |>
    filter(component_data_type == "stock")
  
  faeces_stock_data <- stock_data |>
    filter(cloaca == 0)
  faeces_stock_data <- faeces_stock_data |>
    filter(sample_type == "feces" |
             sample_type == "faeces" & component_weight_type == "dw")
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
  
  a_cnp_fsd <- cnp_fsd |>
    group_by(species_latin_name_gbif, component_name) |>
    dplyr::summarise(
      avg_component_mean = mean(component_mean),
      n_obs = length(component_mean),
      body_mass = first(body_mass),
      diet = first(diet)
    )
  
  # Compute the ratios
  species = unique(a_cnp_fsd$species_latin_name_gbif)
  
  for (i in species) {
    crow = which(a_cnp_fsd$species_latin_name_gbif == i &
                   a_cnp_fsd$component_name == "C")
    nrow = which(a_cnp_fsd$species_latin_name_gbif == i &
                   a_cnp_fsd$component_name == "N")
    prow = which(a_cnp_fsd$species_latin_name_gbif == i &
                   a_cnp_fsd$component_name == "P")
    cn_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/N",
      avg_component_mean = ifelse(
        any(crow) &&
          any(nrow),
        log10(a_cnp_fsd$avg_component_mean[crow] / a_cnp_fsd$avg_component_mean[nrow]),
        NA
      ),
      body_mass = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "C"),]),
                   nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "N"),]))
    )
    cp_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/P",
      avg_component_mean = ifelse(
        any(crow) &&
          any(prow),
        log10(a_cnp_fsd$avg_component_mean[crow] / a_cnp_fsd$avg_component_mean[prow]),
        NA
      ),
      body_mass = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "C"),]),
                   nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "P"),]))
    )
    np_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "N/P",
      avg_component_mean = ifelse(
        any(nrow) &&
          any(prow),
        log10(a_cnp_fsd$avg_component_mean[nrow] / a_cnp_fsd$avg_component_mean[prow]),
        NA
      ),
      body_mass = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "N"),]),
                   nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "P"),]))
    )
    
    # Add Row using rbind()
    a_cnp_fsd = rbind(a_cnp_fsd, cn_row, cp_row, np_row)
  }
  
  
  
  write.csv(a_cnp_fsd,
            here::here("1_data",
                       "a_cnp_fsd.csv"),
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
  
  a_cnp_gsd <- cnp_gsd |>
    group_by(species_latin_name_gbif, component_name) |>
    dplyr::summarise(
      avg_component_mean = mean(component_mean),
      body_mass = first(body_mass),
      diet = first(diet)
    )
  
  # Compute the ratios
  species = unique(a_cnp_gsd$species_latin_name_gbif)
  
  for (i in species) {
    crow = which(a_cnp_gsd$species_latin_name_gbif == i &
                   a_cnp_gsd$component_name == "C")
    nrow = which(a_cnp_gsd$species_latin_name_gbif == i &
                   a_cnp_gsd$component_name == "N")
    prow = which(a_cnp_gsd$species_latin_name_gbif == i &
                   a_cnp_gsd$component_name == "P")
    cn_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/N",
      avg_component_mean = ifelse(
        any(crow) &&
          any(nrow),
        log10(a_cnp_gsd$avg_component_mean[crow] / a_cnp_gsd$avg_component_mean[nrow]),
        NA
      ),
      body_mass = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                        cnp_gsd$component_name == "C"),]),
                   nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                        cnp_gsd$component_name == "N"),]))
    )
    cp_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/P",
      avg_component_mean = ifelse(
        any(crow) &&
          any(prow),
        log10(a_cnp_gsd$avg_component_mean[crow] / a_cnp_gsd$avg_component_mean[prow]),
        NA
      ),
      body_mass = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                        cnp_gsd$component_name == "C"),]),
                   nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                        cnp_gsd$component_name == "P"),]))
    )
    np_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "N/P",
      avg_component_mean = ifelse(
        any(nrow) &&
          any(prow),
        log10(a_cnp_gsd$avg_component_mean[nrow] / a_cnp_gsd$avg_component_mean[prow]),
        NA
      ),
      body_mass = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                        cnp_gsd$component_name == "N"),]),
                   nrow(cnp_gsd[which(cnp_gsd$species_latin_name_gbif == i &
                                        cnp_gsd$component_name == "P"),]))
    )
    
    # Add Row using rbind()
    a_cnp_gsd = rbind(a_cnp_gsd, cn_row, cp_row, np_row)
  }
  

  
  
  
  ### Write data file ###
  
  write.csv(a_cnp_gsd,
            here::here("1_data",
                       "a_cnp_gsd.csv"),
            row.names = FALSE)
  
  data
  
}