#' Run predefined quality check test on a raw dataset
#' 
#' The following function checks for the presence of invalid combinations within a @param dataframe
#' of variables supplied @param condition_list. The @param condition_list must be a dataframe containing
#' at least two variables: @param condition as the name of the condition one wishes to check and @test as 
#' the test applied to each condition.
#'
#' 
#' @param data the dataset you wish to check for invalid combinations of variables
#' @param condition the set of condition you wish to check
#' @param tests the set of test related to each condition
#' @param meta_to_keep a list of metadata you wish to include in the final output
#' 
#' @return Standardized issues table for quality check in long format. 
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




run_checks_from_dataframe<-function(data, conditions_dataframe, condition.column, test.name.column, meta_to_keep = c()){
  
  if(!is.data.frame(data)) {stop("data must be a dataframe")}
  
  if(!is.data.frame(conditions_dataframe)) {stop("conditions_dataframe must a dataframe")}
  
  if(!is.vector(condition.column)) {stop("conditions must be a vector")}
  
  if(!is.vector(test.name.column)) {stop("test name must be a vector")}
  
  if(!(condition.column%in% names(conditions_dataframe))){stop(paste(condition.column, " not a column name in conditions_dataframe"))}
  if(!(test.name.column %in% names(conditions_dataframe))){stop(paste(test.name.column, " not a column name in conditions_dataframe"))}
  
  conditions <- conditions_dataframe[[condition.column]]
  tests <- conditions_dataframe[[test.name.column]]
  batch_issue_checks(data,conditions = conditions,tests = tests,meta_to_keep = meta_to_keep)
  
}