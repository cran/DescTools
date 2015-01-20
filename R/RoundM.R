RoundM <-
function(x, multiple = 1, FUN = round) { 
  
  # check for functions: round, ceiling, floor, but how????
  # FUN <- match.arg(FUN, c(round, ceiling, floor))
  
  if(is.function(FUN)) { 
    # if FUN is a function, then save it under new name and 
    # overwrite function name in FUN, which has to be character
    fct <- FUN
    FUN <- "fct"
    FUN <- gettextf("%s", FUN)
  }

  # round will set digits to 0 by default, which is exactly what we need here  
  return(eval(parse(text = gettextf("%s(x/multiple) * multiple", FUN))))  
}
