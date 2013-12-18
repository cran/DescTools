Desc.integer <-
function(x, xname = NULL, maxlevels = 12
  , digits = 3, plotit = FALSE, ... ) {


  if( is.null(xname)) xname <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  uvals <- length(unique(na.omit(x)))
  
  if(uvals == 2) {
    # if there are only 2 unique values, describe it as dichotomous variable
    lres <- Desc.logical(x, xname=gsub(pattern="integer)", replacement="integer - dichotomous)", x=xname)) 

  } else {
    # produce the numeric summary
    lres <- Desc.numeric(x, xname=xname, highlow = FALSE) 
    
    if(length(na.omit(x))>0){  # following only if there are valid values
      # if less than maxlevels unique values produce a full frequency table...
      if(is.na(maxlevels) || uvals <= maxlevels) { 
        frq <- Freq( x = as.factor(x), digits = digits, print = FALSE)
  
        txt.frq <- capture.output(frq)
        txt.frq <- paste(substr(txt.frq, 0, regexpr("level", txt.frq[1]) + 5), 
                    gsub(pattern="0\\.", replacement=" \\.", 
                         x=substr(txt.frq, regexpr("level", txt.frq[1])+5+1, nchar(txt.frq[1]))), 
                    sep="")
        
        cat("\n")
        cat(txt.frq, sep = "\n")
        cat("\n")
        lres$frq <- frq
      } else {
        # ... else give only highlow values    
        txt.highlow <- HighLow(x)
        cat(txt.highlow, "\n", sep="")
      }
    }
  }
  
  if(plotit) PlotDesc.integer(x, main=xname, maxrows=maxlevels)

  invisible(lres)
  
}
