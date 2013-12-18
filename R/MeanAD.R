MeanAD <-
function(x, FUN = mean, na.rm = FALSE) {

  if (na.rm) x <- na.omit(x)
  
  if(is.function(FUN)) { 
    #  if FUN is a function, then save it under new name and 
    # overwrite function name in FUN, which has to be character
    fct <- FUN
    FUN <- "fct"
  }
  # Calculates the mean absolute deviation from the sample mean.  
  return(eval(parse(text = gettextf("mean(abs(x - %s(x)))", FUN))))  
}
