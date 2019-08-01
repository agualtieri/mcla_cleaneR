reformat_quality_checks <- function(data, var_to_separate, sep1, sep2 = NULL, sep3 = NULL){
  
  require(splitstackshape)
  
  x <- splitstackshape::cSplit(data, var_to_separate, sep = sep1, "long")
  
  if (!is.null(sep2)) {
    
  y <- splitstackshape::cSplit(x, var_to_separate, sep = sep2, "long")  
    
  } else if (!is.null(sep3)) {
    
    z <- splitstackshape::cSplit(y, var_to_separate, sep = sep3, "long")
    z
    
  }
  

}



