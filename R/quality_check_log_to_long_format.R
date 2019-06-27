#' Function to melt quality check log from long to wide
#' 
#' The following function turns the quality check output from wide to long format.
#' 
#' @param data the dataset you wish to transform from wide to long
#' @param variable_name the name of the measured variable. The default name is 'variable'
#' @param value_name name of the transformed data value. The default name is 'value'
#' @param meta_not_to_transform a list of metadata you wish to keep as id variables.
#' 
#' @return A transformed quality check log
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


quality_checks_log_to_long_format <- function(data, variable_name = "variable" , value_name = "value", meta_not_to_transform = c()){
  
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.character(variable_name))
  assertthat::assert_that(is.character(value_name))
  assertthat::assert_that(is.vector(meta_not_to_transform))
  
  
  ## reshape data into long format using melt
  data_long <- reshape2::melt(data, variable.name = variable_name, value.name = value_name, id.vars = meta_not_to_transform, na.rm = T) %>% as_tibble
  
  assertthat::assert_that(is.data.frame(data_long))
  
  ## remove all zeros
  #data_long <- data_long %>% filter(value_name != 0) %>% as_tibble
  
  data_long[apply(data_long != 0, 1, all),]
  
  data_long 
}







