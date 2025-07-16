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
  
  # Check that species names in the data traits database are up to date
  
  species = na.omit(unique(data_n$species_latin_name_gbif))
  
  if (all(data_t$species_names %in% species) == F) {
    message("⚠️  The trait database contains outdated species names.\n")
    
    message("❗ The following species names are outdated:")
    print(data_t[which(data_t$species_names %in% species == F ), ]$species_names)
    
    message(
      "🔧 Open the data_traits table to put back the right species names."
    )
    
    stop("❌ Execution halted due to outdated species names in the data_traits database")
  }
  
  
  # We keep species_name, body mass and diet to add the the data_nutrients dataframe
  data_traits = data_t
  data_nutrients = data_n
  data_traits = data_traits[, c("species_names", "body_mass", "diet")]
  
  body_mass = NA
  diet = NA
  data = cbind(data_nutrients, body_mass, diet)
  species = na.omit(unique(data_nutrients$species))
  n_species = length(species)
  
  for (i in species) {
    row_species_in_data = which(data$species == i)
    row_species_in_data_traits  = which(data_traits$species == i)
    species_body_mass = data_traits[row_species_in_data_traits, ]$body_mass
    species_diet = data_traits[row_species_in_data_traits, ]$diet
    data[row_species_in_data, ]$body_mass = species_body_mass
    data[row_species_in_data, ]$diet = species_diet
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