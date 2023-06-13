################################ NETSTO PROJECT ################################

#' load_df
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

load_df  = function(path) {

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
