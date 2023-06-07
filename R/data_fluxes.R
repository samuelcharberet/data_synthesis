################################ NETSTO PROJECT ################################

#' data_fluxes_structuration
#'
#' @param path
#'
#' @return a clean table with added taxonomic data from the Global Biodiversity Information Facility
#' @export
#'
#' @examples
#' @authors Samuel Charberet
#'
#'

data_nutrient_structuration = function(path) {
  path = here::here("1_data", "2_data_fluxes", "data_fluxes.csv")
  data_fluxes = read.csv(file = path)
  
  # Structuring the data fluxes table
  
  data_fluxes$Feed = factor(
    data_fluxes$Feed,
    levels = c(
      "Herbivore",
      "Omnivore",
      "Carnivore",
      "Insectivore",
      "Frugivore",
      "Nectarivore"
    )
  )
  
  data_fluxes$Species_lat =  tolower(data_fluxes$Species_lat)
  
  data_fluxes
  
}
