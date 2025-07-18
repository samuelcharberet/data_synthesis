################################ NETSTO PROJECT ################################

#' load_dt
#'
#' @param path
#'
#' @return a file containing traitdata
#' @export
#'
#' @examples
#' @authors Samuel Charberet

load_dt = function(path) {
  df_t = read.csv(path)
  df_t
}