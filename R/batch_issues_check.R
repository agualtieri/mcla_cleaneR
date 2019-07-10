#' Batch quality check of predefined conditions
#' 
#' The following function checks for the presence of invalid combinations of variables supplied @param condition
#' within the supplied @param data and produced and editable output that can be used for data quality checks.
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
#' batch_issue_checks()
#'
#' @export
#



batch_issue_checks <- function(data, conditions, tests, meta_to_keep = c()){
  
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.vector(meta_to_keep))
  assertthat::assert_that(is.character(conditions))
  assertthat::assert_that(is.character(tests))
  
  
  data_with_issues <- data %>% recode_batch(tos = rep(1,length(conditions)),
                                            wheres = conditions,
                                            targets = tests) %>% end_recoding
  
  
  unique_targets <- unique(tests) 
  
  data_with_issues[,unique_targets] <- lapply(data_with_issues[,unique_targets], function(x){
    x[is.na(x)] <-0
    x }) %>% as_tibble
  
  data_with_issues %>% select(c(meta_to_keep, unique_targets))
  
}

