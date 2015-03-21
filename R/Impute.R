Impute <-
function(x, FUN = function(x) median(x, na.rm=TRUE)) {
  
  if(is.function(FUN)) { 
    #  if FUN is a function, then save it under new name and 
    # overwrite function name in FUN, which has to be character
    fct <- FUN
    FUN <- "fct"
    FUN <- gettextf("%s(x)", FUN)
  }
  # Calculates the mean absolute deviation from the sample mean.  
  return(eval(parse(text = gettextf("replace(x, is.na(x), %s)", FUN))))  
  
}
