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
  

  
}
