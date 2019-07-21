#'
#'
#'Split strings on multiple delimters 
#'
#' @param x string or vector of strings
#' @param separator single or vector of separator delimters
#' @return a list of vectors with split strings
#' @details does not support regex matching
#' @examples
#' strings_to_split<-c('abc def.gh', "def.kl abc 9")
#' separation_pattern<-c(" ", "c")
#' strsplit_on_multiple(x = strings_to_split,
#'                      separation_pattern)
#' 
#'
strsplit_on_multiple<-function(x,separator){
  
  separator<- Hmisc::escapeRegex(separator)
  
  collapsed_separator<-paste0( separator, collapse= "|")
  split_element<- strsplit(x=x,split = collapsed_separator) %>%
    lapply(function(x) x[x!=""]) #remove any empty returned strings
  split_element %>% do.call(rbind, .)
}



string_to_split <- clog_separated$quality_checks
separator <- c("==", "!=")

test <- strsplit_on_multiple(clog_separated$quality_checks, separator)

