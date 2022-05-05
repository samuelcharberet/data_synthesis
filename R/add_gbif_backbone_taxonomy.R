#' add_gbif_backbone_taxonomy
#'
#' @param dataframe 
#' @param speciescolumn 
#'
#' @return a dataframe with added GBIF backbone taxonomy data
#' A function to download data from the GBIF Backbone Taxonomy and combine
# it with a given dataframe containing a column with species names
#' @export
#'
#' @examples
#' 
add_gbif_backbone_taxonomy = function(dataframe, speciescolumn) {
  # Standardize species name
  speciescolumn_name = as.character(speciescolumn) # we define the class of speciesname as a character
  species_column_index = which(names(dataframe) == speciescolumn_name) # the column number
  
  dataframe[, speciescolumn] = tolower(dataframe[, speciescolumn])
  dataframe[, speciescolumn] = gsub(" ", "_", dataframe[, speciescolumn])
  dataframe = dataframe[which(dataframe[, speciescolumn] != ''), ] # remove rows where there are no given species name
  list_sp = unique(dataframe[, speciescolumn])
  list_data_gbif = get_gbifid_(list_sp) # we download information from the GBIF Backbone Taxonomy
  
  # we search for the maximum number of columns in all the dataframes in the list data gbif
  list_ncol = NULL
  for (i in 1:length(list_data_gbif)) {
    list_ncol = c(list_ncol, ncol(list_data_gbif[[i]]))
  }
  
  ligne_na = rep(NA, length.out = max(list_ncol))
  names(ligne_na) = names(list_data_gbif[[which(list_ncol == max(list_ncol))[1]]])
  
  
  data_gbif = NULL
  
  for (i in 1:length(list_data_gbif)) {
    species_lines = which(list_data_gbif[[i]]$rank == "species")
    list_data_gbif[[i]] = list_data_gbif[[i]][species_lines,]
    accepted_rows = which(list_data_gbif[[i]]$status == "ACCEPTED")
    synonym_rows = which(list_data_gbif[[i]]$status == "SYNONYM")
    exact_rows = which(list_data_gbif[[i]]$matchtype == "EXACT")
    fuzzy_rows = which(list_data_gbif[[i]]$matchtype == "FUZZY")
    if (nrow(list_data_gbif[[i]]) == 0) {
      data_gbif = bind_rows(data_gbif, ligne_na)
    }
    else if (nrow(list_data_gbif[[i]]) == 1) {
      data_gbif = bind_rows(data_gbif, list_data_gbif[[i]])
    }
    else if (nrow(list_data_gbif[[i]]) > 1) {
      if (length(intersect(accepted_rows, exact_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence))[1]
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
      else if (length(intersect(synonym_rows, exact_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence))[1]
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
      else if (length(intersect(accepted_rows, fuzzy_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence))[1]
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
      else if (length(intersect(synonym_rows, fuzzy_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence))[1]
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
    }
  }
  
  list_nrow = NULL
  for (i in 1:length(list_data_gbif)) {
    list_nrow = c(list_nrow, nrow(list_data_gbif[[i]]))
  }
  length(which(list_nrow == 0))
  
  # Only keep data from accepted status names
  data_gbif = data_gbif[which(data_gbif$status == "ACCEPTED"), ]
  
  # Only keep data from exact matches
  data_gbif = data_gbif[which(data_gbif$matchtype == "EXACT"), ]
  
  # Combine the GBIF Backbone Taxonomy data with the data #
  
  # select relevant data from the GBIF data
  useful_data_gbif =  data_gbif[c(
    "canonicalname",
    "rank",
    "status",
    "matchtype",
    "species",
    "specieskey",
    "phylum",
    "class",
    "order",
    "family",
    "genus"
  )]
  colnames(useful_data_gbif) = c(
    "canonical_name",
    "rank",
    "status",
    "match_type",
    "species",
    "specieskey",
    "phylum",
    "class",
    "order",
    "family",
    "genus"
  )
  colnames(useful_data_gbif) = paste0(colnames(useful_data_gbif), '_gbif')
  
  # Add columns to the data table
  
  dataframe[, colnames(useful_data_gbif)] = NA
  
  
  # Standardize species name
  
  useful_data_gbif$species_gbif = tolower(useful_data_gbif$species_gbif)
  useful_data_gbif$species_gbif = gsub(" ", "_", useful_data_gbif$species_gbif)
  
  # A for loop to combine the GBIF data with the datatable
  
  for (i in 1:nrow(dataframe)) {
    if (!is.na(dataframe[, speciescolumn][i])) {
      if (length(which(useful_data_gbif$species_gbif == dataframe[, speciescolumn][i])) >
          0) {
        dataframe[i , colnames(useful_data_gbif)] =
          useful_data_gbif[which(useful_data_gbif$species_gbif == dataframe[, speciescolumn][i]), colnames(useful_data_gbif)]
      }
    }
  }
  return(dataframe)
}
