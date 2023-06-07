################################ NETSTO PROJECT ################################

#' load_dnl
#'
#' @param paths
#'
#' @return a list of files containing data from the literature
#' @export
#'
#' @examples
#' @authors Samuel Charberet


load_dnl = function(paths) {
  
  # Asks the list of all directories
  # dir(here::here("1_data", "1_data_nutrients", "1_data_literature", "1_data"))
  ######  1. Read nutrients data tables #####
  
  # Make a list of xls(x) files
  
  n_files = length(paths) # the number of dataframes
  
  # We may want to check manually if the number of elements in 'paths' matches the
  # number of tables in the database
  
  # Create a list of dataframes containing each literature datatable
  list_df_dl <- lapply(paths, readxl::read_excel, na = "NA")
  
  ######  2. Check that all tables have the same structure ######
  
  # 2.1 Do all tables have the same number of column
  
  ncol_database = 82 # In the version of (2021/02/22), the database has 82 columns
  list_ncol = lapply(list_df_dl, ncol)
  wrong_ncol = which(list_ncol != ncol_database, arr.ind = TRUE) # Which data frame does NOT have the correct number of columns ?
  if (length(wrong_ncol) >  0) {
    print(paths[wrong_ncol]) # the name of the files that does not have the correct number of columns
    stop("Not all files have the correct number of columns")
  }
  rm(list_ncol, wrong_ncol)
  
  
  # 2.2 Do all tables have the same names of columns and in the same order ?
  list_colnames = lapply(list_df_dl, names)
  for (i in 1:ncol_database) {
    for (j in 1:n_files) {
      if (list_colnames[[1]][i] != list_colnames[[j]][i]) {
        print(paths[j])
        print(i)
        stop("Not all files have the same names of columns in the same order")
      }
    }
  }
  rm(paths, list_colnames, n_files, ncol_database)
  
  list_df_dl
}
