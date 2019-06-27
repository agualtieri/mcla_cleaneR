#' Function to anonymise dataset
#' 
#' The following function removes all the assigned sensitive informations from the raw dataset
#' 
#' @param data the dataset you wish to anonymise
#' @param variables_to_remove the names of the sensitive variables you want to remove

#' 
#' @return An anonymised dataset
#' 
#' 
#' @author Alberto Gualtieri, \email{alberto.gualtieri@@reach-initiative.org}
#' @references \url{https://github.com/agualtieri/mcla_cleaneR}
#' @keywords yemen, mcla, quality check, platypus
#' 
#'
#' @examples
#' run_checks_from_dataframe()
#'
#' @export


anonymise_dataset <- function(data, variables_to_remove=c()){
  
  data_anon <- data[, !(names(data) %in% variables_to_remove)]
  
  data_anon
}


