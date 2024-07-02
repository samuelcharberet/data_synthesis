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
  
  # Re-order the diet factor
  data$diet = str_to_title(data$diet)
  data$diet = factor(data$diet,
                     levels = c('Herbivore', 'Omnivore', 'Carnivore', 'Detritivore'))
  
  # Create the full data file
  write.csv(data, here::here("1_data", "data.csv"), row.names = FALSE)
  
  return(data)
  
}