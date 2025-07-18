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
    colnames(data_fluxes)[1] = "Diet"
    
  # Structuring the data fluxes 
    
  data_fluxes
  data_fluxes$Diet = factor(
    data_fluxes$Diet,
    levels = c(
      "Herbivore",
      "Omnivore",
      "Carnivore"
    )
  )
  
  data_fluxes$Species_lat =  tolower(data_fluxes$Species_lat)
  
  data_fluxes
  
}
