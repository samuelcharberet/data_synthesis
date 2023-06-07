################################ NETSTO PROJECT ################################


# date : 11/08/2021

#' data_nutrient_structuration
#'
#' @param path 
#'
#' @return a table resulting from the vertical merge of tables from the NetSto nutrient database 
#' with added taxonomic data from the Global Biodiversity Information Facility
#' @export
#'
#' @examples
#' @authors Anne-Cecile vain, Lucie Serre, Jerome Mathieu, Samuel Charberet
#' 
data_nutrient_structuration = function(path) {
  
  # Ask the list of all directories
  # dir(here::here("1_data", "1_data_nutrient", "1_data_literature", "1_data"))
  
  ######  1. Read nutrient data tables #####
  
  # Make a list of xls(x) files
  files <-
    list.files(pattern = "[0-9]{4}_[0-9]{3}_.*?_[0-9]{4}.xls",
               recursive = TRUE)
  
  n_files = length(files) # the number of dataframes
  
  # We may want to check manually if the number of elements in 'files' matches the
  # number of tables in the database
  
  # Create a list of dataframes containing each literature datatable
  list_files <- lapply(files, readxl::read_excel, na = "NA")
  
  ######  2. Check that all tables have the same structure ######
  
  # 2.1 Do all tables have the same number of column
  
  ncol_database = 82 # In the version of (2021/02/22), the database has 82 columns
  list_ncol = lapply(list_files, ncol)
  wrong_ncol = which(list_ncol != ncol_database, arr.ind = TRUE) # Which data frame does NOT have the correct number of columns ?
  
  if (length(wrong_ncol) > 0) {
    warning("Not all files have the same number of columns")
    warning("The following files do note have the same number of column")
    print(files[wrong_ncol])
  }
  rm(list_ncol, wrong_ncol)
  
  
  # 2.2 Do all tables have the same names of columns and in the same order ?
  list_colnames = lapply(list_files, names)
  for (i in 1:ncol_database) {
    for (j in 1:n_files) {
      if (list_colnames[[1]][i] != list_colnames[[j]][i]) {
        warning("Not all files have the same names of columns in the same order")
        print(files[j])
        print(i)
      }
    }
  }
  rm(files, list_colnames, n_files, ncol_database)
  
  
  ######  3. Vertical concatenation of all tables  ######
  
  data_nutrient_literature <- plyr::rbind.fill(list_files)
  rm(list_files)
  # Add lab original data
  data_nutrient_samples = readxl::read_excel(
    here::here(
      "1_data",
      "1_data_nutrient",
      "2_data_samples",
      "1030_999_charberet_2020",
      "1030_999_charberet_2020.xlsx"
    ),
    na = "NA"
  )
  
  data_nutrient = plyr::rbind.fill (data_nutrient_literature, data_nutrient_samples)
  rm(data_nutrient_literature, data_nutrient_samples)
  data_nutrient = data_nutrient[rowSums(is.na(data_nutrient)) != ncol(data_nutrient), ] # We remove rows containing only NAs
  
  ######  4. Homogenize the nomenclature of the species variables ####
  
  data_nutrient$species_latin_name_gbif = tolower(data_nutrient$species_latin_name_gbif)
  data_nutrient$species_latin_name_gbif = gsub(" ", "_", data_nutrient$species_latin_name_gbif)
  
  ###### 5. Remove rows where there are no GBIF_ID in the database ###
  
  data_nutrient = data_nutrient[-which(is.na(data_nutrient$gbif_id)), ]
  #check in the original table if needed
  
  ##### 6. Finding where there are GBIF_ID given to a taxonomic level higher than species ####
  
  gbif_ids = unique(data_nutrient$gbif_id)
  list_rank = id2name(gbif_ids, db = "gbif")
  rm(gbif_ids)
  rank_notspecies = which(sapply(list_rank, function(e)
    ! is.element('species', e)))
  not_species_id = as.integer(names(list_rank[rank_notspecies]))
  rm(list_rank, rank_notspecies)
  if (length(which(is.na(not_species_id))) > 0) {
    not_species_id = not_species_id[-which(is.na(not_species_id))]
  }
  
  ##### 7. Remove data with taxonomic level higher than species ####
  
  for (i in 1:length(not_species_id)) {
    data_nutrient = data_nutrient[-which(data_nutrient$gbif_id == not_species_id[i]), ]
  }
  rm(not_species_id)
  
  ##### 8. Create a table with taxonomic data for each species  ####
  
  test = add_gbif_backbone_taxonomy(data_nutrient, "species_latin_name_gbif")
  
  # creates a character object with n element, n the number of unique species name
  list_sp <-
    unique(data_nutrient$species_latin_name_gbif)
  
  # Remove NA from the list
  list_sp = list_sp[-which(is.na(list_sp))]
  # There are x different species names in the raw nutrient database
  # Get the GBIF taxonomic data for each species from their taxonomic names
  # get_gbifid_ is a function to search GBIF's taxonomy
  # if an exact match is found, the ID for that match is returned
  # If there isn't an exact match, the options are returned in a dataframe, and we have to pick up one.
  # The match can be either exacvt or fuzzy, and the name can either be synonym or accepted
  # Here we create a list of data frames containing one or more match proposals
  
  list_data_gbif = get_gbifid_(gsub("_", " ", list_sp))
  rm(list_sp)
  # Create a table with taxonomic data for each species
  # This table is supposed to have the same number of rows as the number of unique species names in data_nutrient
  # We select in priority exact matches and accepted species names, then synonym species names.
  # If there are no exact but only fuzzy matches ,we select in priority accepted species names and then synonym species names
  
  data_gbif <- NULL
  
  for (i in 1:length(list_data_gbif)) {
    if (nrow(list_data_gbif[[i]]) == 1) {
      data_gbif <- bind_rows(data_gbif, list_data_gbif[[i]])
    } else if (nrow(list_data_gbif[[i]]) == 0) {
      ligne_na = rep(NA, length.out = length(list_data_gbif[[1]]))
      names(ligne_na) = names(list_data_gbif[[1]])
      
      data_gbif = bind_rows(data_gbif, ligne_na)
    } else {
      accepted_rows = which(list_data_gbif[[i]]$status == "ACCEPTED")
      synonym_rows = which(list_data_gbif[[i]]$status == "SYNONYM")
      exact_rows = which(list_data_gbif[[i]]$matchtype == "EXACT")
      fuzzy_rows = which(list_data_gbif[[i]]$matchtype == "FUZZY")
      if (length(intersect(accepted_rows, exact_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence)[1])
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
      else if (length(intersect(synonym_rows, exact_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence)[1])
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
      else if (length(intersect(accepted_rows, fuzzy_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence)[1])
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
      else if (length(intersect(synonym_rows, fuzzy_rows)) > 0) {
        max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence ==
                           max(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence)[1])
        data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
      }
    }
  }
  rm(
    list_data_gbif,
    accepted_rows,
    exact_rows,
    fuzzy_rows,
    synonym_rows,
    ligne_na,
    max_conf
  )
  
  factor_columns = c("rank", "status", "matchtype")
  data_gbif[factor_columns] = lapply(data_gbif[factor_columns], factor)
  rm(factor_columns)
  
  ##### 9. Finding the subspecies if there are some ####
  
  subspecies = data_gbif[which(data_gbif$rank == "subspecies"), "canonicalname"] # find subspecies canonical names
  # If subspecies is empty, then it means that there are no subspecies in the dataset
  subspecies = as.character(subspecies)
  subspecies = tolower(subspecies)
  subspecies =  gsub(" ", "_", subspecies)
  
  if (length(subspecies) > 1) {
    warning("The database nutrient contains subspecies names instead of species binomial names")
    warning("The following tables contain subspecies ID")
    print(unique(data_nutrient[which(data_nutrient$species_latin_name_gbif == subspecies),]$reference_ID))
    warning("The following subspecies are named")
    warning(
      "Open the table associated with these references ID to put back the species names instead of subspecies"
    )
    print(subspecies)
  }
  rm(subspecies)
  
  
  ##### 10. Check the status of the scientific name  ####
  
  
  synonym = data_gbif[which(data_gbif$status == "SYNONYM"),]$canonicalname # find synonym species canonical names
  # If synonym is empty, then it means that there are no synonym species in the dataset
  synonym = as.character(synonym)
  synonym = tolower(synonym)
  synonym =  gsub(" ", "_", synonym)
  
  if (length(synonym) > 1) {
    warning("The database nutrient contains incorrect names.")
    warning("The following tables contain incorrect ID")
    print(unique(data_nutrient[which(data_nutrient$species_latin_name_gbif == synonym),]$reference_ID))
    warning("The following incorrect names are given")
    warning("Open the table associated with these references ID to put back the right species names")
    print(synonym)
  }
  rm(synonym)
  
  ##### 11. Check the match types ####
  
  fuzzy = data_gbif[which(data_gbif$matchtype == "FUZZY"),]$canonicalname # find fuzzy matches canonical names
  # If fuzzy is empty, then it means that there are no synonym species in the dataset
  fuzzy = as.character(fuzzy)
  fuzzy = tolower(fuzzy)
  fuzzy =  gsub(" ", "_", fuzzy)
  
  if (length(fuzzy) > 1) {
    warning("The database nutrient contains fuzzy names.")
    warning("The following tables contain fuzzy names")
    
    print(unique(data_nutrient[which(data_nutrient$species_latin_name_gbif == fuzzy),]$reference_ID)) # find references
    warning("The following fuzzy names are given")
    warning("Open the table associated with these references ID to put back the right species names")
    print(fuzzy)
  }
  rm(fuzzy)
  
  character_columns = c("rank", "status", "matchtype")
  data_gbif[character_columns] = lapply(data_gbif[character_columns], as.character)
  rm(character_columns)
  ##### 12. Check that no GBIF species keys present in data nutrient and absent from data gbif ####
  data_no_match = NULL
  
  for (i in 1:nrow(data_nutrient)) {
    # If the line GBIF_ID is not a NA
    if (!is.na(data_nutrient$gbif_id[i])) {
      # which species is it in the data_gbif
      a = data_nutrient$gbif_id[i] == data_gbif$specieskey
      # If
      if (length(which(a == T)) == 0) {
        data_no_match = rbind(data_no_match, data_nutrient[i,])
      }
    }
  }
  if (is.null(data_no_match)) {
    warning("There are no GBIF species key present in data_nutrient and absent from data_gbif")
  } else {
    warning("There are species key present in data_nutrient and absent from data_gbif")
  }
  
  data_nutrient = data_nutrient[-as.numeric(rownames(data_no_match)), ]
  rm(data_no_match)
  
  #### 13. Combine the GBIF data with the nutrient data  ####
  
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
  
  
  colnames(useful_data_gbif) <-
    c(
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
  
  # Add columns to the data_nutrient table
  data_nutrient[, c(
    "canonical_name",
    "rank",
    "status",
    "match_type",
    "species",
    "phylum",
    "class",
    "order",
    "family",
    "genus"
  )] = NA
  
  # A for loop to combine the GBIF data with the nutrient data
  
  for (i in 1:nrow(data_nutrient)) {
    if (!is.na(data_nutrient$gbif_id[i])) {
      if (length(which(useful_data_gbif$specieskey == data_nutrient$gbif_id[i])) >
          0) {
        data_nutrient[i , c(
          "canonical_name",
          "rank",
          "status",
          "match_type",
          "species",
          "gbif_id",
          "phylum",
          "class",
          "order",
          "family",
          "genus"
        )] =
          useful_data_gbif[which(useful_data_gbif$specieskey == data_nutrient$gbif_id[i])[1],
                           c(
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
                           )]
      }
    }
  }
  
  
  ######  14. Write a data file ######
  
  write.csv(
    data_nutrient,
    here::here(
      "1_data",
      "1_data_nutrient",
      "3_data_nutrient_combined",
      "data_nutrient_combined.csv"
    ),
    row.names = FALSE
  )
  
}
