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
combine_nutrients_traits = function(data_nutrients, data_traits) {
  # We first generate a blank template table so that it can be filled by hand
  
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
  
  # We keep species_name, body mass and diet to add the the data_nutrients dataframe
  
  
  data_traits = data_traits[, c("species_names", "body_mass", "diet")]
  
  body_mass = NA
  diet = NA
  data = cbind(data_nutrients, body_mass, diet)
  
  
  for (i in 1:n_species) {
    row_species = which(data$species == species[i])
    species_body_mass = data_traits[i,]$body_mass
    species_diet = data_traits[i,]$diet
    data[row_species,]$body_mass = species_body_mass
    data[row_species,]$diet = species_diet
  }
  
  
  write.csv(
    data,
    here::here("1_data",
               "data.csv"),
    row.names = FALSE
  )
  
  data
  
}