### Quality check function

#### Generalized version of the quality check script - valid for only 2 options

#' find combinations of values
#' @param .data the dataset
#' @param variable1 the name of the first variable
#' @param variable1_values the values of the first variable in the combinations that should be found
#' @param variable2 the name of the second variable
#' @param variable2_values the values of the second variable in corresponding directly to the values in the first variable that toghether make the combinations we are looking for
#' @value logical vector: for each row in the .data, was the combination found?

## Load issues sheet
mcla_issues_sheet <- read.csv("data/mcla_issues_sheet.csv", stringsAsFactors = F)


find_combinations<-function(.data, variable1, variable1_values, variable2, variable2_values){
  
  combinations_found <- purrr::pmap(.l = list(variable1_values,variable2_values), function(value1,value2,variable1, variable2){
    ifelse(.data[[variable1]]==value1 & .data[[variable2]]==value2,TRUE,FALSE)  
  }) %>% do.call(cbind,.) %>% apply(1,any)
  
  combinations_found
  
}



