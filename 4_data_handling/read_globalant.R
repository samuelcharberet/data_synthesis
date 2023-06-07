#' read_globalant
#' A function to extract data from the
# Global ant trait database
# The function standardize species taxonomy according to the GBIF Backbone Taxonomy
# And we keep only one row per species and per caste
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
read_globalant = function(path) {
  setwd(path)
  
  ##### 1. Imports the Global ant database #####
  
  data_globalant = read.csv("global_ant_trait.csv")
  
  # Create a species binomial name column
  data_globalant$binomial = paste(data_globalant$Genus, data_globalant$Species)
  
  # Change column names
  
  names(data_globalant) = gsub("\\.*?$", "", names(data_globalant)) # removes points at the end of column names
  names(data_globalant) = gsub("\\.\\.", "_", names(data_globalant))
  names(data_globalant) = gsub("\\.", "_", names(data_globalant))
  
  # Replace by NA where data is lacking
  
  data_globalant[which(data_globalant == "Not in paper", arr.ind = T)] =
    NA
  
  ##### 2. Add data from the GBIF Backbone Taxonomy #####
  
  data_globalant = add_gbif_backbone_taxonomy(dataframe = data_globalant, speciescolumn =
                                                "binomial")
  
  
  ##### 4. Keep only one row per species and caste #####
  
  # We remove from data_globalant rows which did not find corresponding names in data_gbif
  data_globalant = data_globalant[-which(is.na(data_globalant$species_gbif)),]
  
  # Removes global ant taxonomic data to only keep GBIF data
  # Removes data unused in the NetSto project
  
  data_globalant = subset(
    data_globalant,
    select = -c(
      Visibility,
      Locality_ID,
      Traits_ID,
      Genus,
      Species,
      Contributor,
      Head_width_across_eyes_mm,
      Head_length_mm,
      Clypeus_length_mm,
      Mandible_length_mm,
      Hind_femur_length_mm,
      Scape_length_mm,
      Weber_s_length_mm,
      Pronotum_width_mm,
      Inter_ocular_width_mm,
      Max_eye_width_mm,
      Sculpturing,
      Pilosity,
      Number_of_Spines,
      Dominant_colour_head_Fig_16,
      Dominant_colour_mesosoma_Fig_16,
      Dominant_colour_gaster_Fig_16,
      Polymorphism,
      Queen_number,
      Worker_number,
      Colony_type,
      Colony_founding,
      Notes,
      binomial
    )
  )
  
  
  
  # Here we can compute averages as we have the N
  # We compute one average for each species and each cast
  
  list_sp = unique(data_globalant$species_gbif)
  
  if (length(which(is.na(list_sp))) > 0) {
    list_sp = list_sp[-which(is.na(list_sp))]
  }
  
  names(data_globalant)
  data_globalant_aggregated = NULL
  mat_na = 1 * !is.na(data_globalant) # Where are data
  col_choice = names(data_globalant)[c(
    which(names(data_globalant) == c("Latitude")),
    which(names(data_globalant) == c("Longitude")),
    which(names(data_globalant) == c("Nest_Site")),
    which(names(data_globalant) == c("Activity")),
    which(names(data_globalant) == c("Diet"))
  )]
  
  col_cont = names(data_globalant)[c(which(names(data_globalant) == c("Whole_body_length_mm")))]
  row_data_nb = rep(NA, nrow(data_globalant))
  
  for (i in 1:length(list_sp)) {
    lines_species = which(data_globalant$species_gbif == list_sp[i]) # select the lines of the current species
    list_caste =  unique(data_globalant[lines_species,]$Caste)
    
    for (j in 1:length(list_caste)) {
      lines_caste = lines_species[which(data_globalant[lines_species, ]$Caste ==
                                          list_caste[j])] # select the lines of the current caste
      if (length(lines_caste) == 1) {
        data_globalant_aggregated = bind_rows(data_globalant_aggregated, data_globalant[lines_caste,])
      } # if there is only on row for the current species and caste, we keep that row
      
      else {
        combined_row = data_globalant[lines_caste[1],]
        
        for (k in col_choice) {
          if (sum(mat_na[lines_caste, k]) == 0) {
            combined_row[1, k] = NA
          }
          else if (sum(mat_na[lines_caste, k]) == 1) {
            combined_row[1, k] = data_globalant[lines_caste[which(mat_na[lines_caste, k] ==
                                                                    1)], k]
          }
          else if (sum(mat_na[lines_caste, k]) > 1) {
            for (m in lines_caste) {
              row_data_nb[m] = sum(mat_na[m, col_choice])
            }
            combined_row[1, k] = data_globalant[lines_caste[which(row_data_nb[lines_caste] ==
                                                                    max(row_data_nb[lines_caste]))][1], k]
          }
        }
        
        for (l in col_cont) {
          if (sum(mat_na[lines_caste, l]) == 0) {
            combined_row[1, l] = NA
          }
          else if (sum(mat_na[lines_caste, l]) == 1) {
            combined_row[1, l] = data_globalant[lines_caste[which(mat_na[lines_caste, l] ==
                                                                    1)], l]
          }
          else if (sum(mat_na[lines_caste, l]) > 1) {
            combined_row[1, l] = weighted.mean(as.numeric(data_globalant[lines_caste, col_cont]),
                                               as.numeric(data_globalant[lines_caste, "N"]),
                                               na.rm = T)
            
          }
        }
      }
      
      data_globalant_aggregated = bind_rows(data_globalant_aggregated, combined_row[1, ])
      
    }
  }
  
  names(data_globalant_aggregated) = tolower(names(data_globalant_aggregated))
  
  return(data_globalant_aggregated)
}