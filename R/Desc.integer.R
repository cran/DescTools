Desc.integer <-
function(x, xname = NULL, maxrows = 12, freq = NULL
  , digits = 3, plotit = FALSE, ... ) {


  if( is.null(xname)) xname <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  uvals <- length(unique(na.omit(x)))
  
  if(is.null(freq)) freq <- uvals <= 12
  
  if(uvals == 2) {
    # if there are only 2 unique values, describe it as dichotomous variable
    lres <- Desc.logical(x, xname=gsub(pattern="integer)", replacement="integer - dichotomous)", x=xname)) 

  } else {
    # produce the numeric summary
    lres <- Desc.numeric(x, xname=xname, highlow = FALSE) 
    
#   old:  if(length(na.omit(x))>0){  # following only if there are valid values
#    but we need not recalculate...
    if(lres$n > 0){  # following only if there are valid values
        # if less than maxlevels unique values produce a full frequency table...
      if(freq) { 
        frq <- Freq(x = as.factor(x))
  
        opt <- options(digits=digits)
        txt.frq <- capture.output(frq)
        options(opt)
        
        txt.frq <- paste(substr(txt.frq, 0, regexpr("level", txt.frq[1]) + 5), 
                    gsub(pattern="0\\.", replacement=" \\.", 
                         x=substr(txt.frq, regexpr("level", txt.frq[1])+5+1, nchar(txt.frq[1]))), 
                    sep="")
        
        cat("\n")
        cat(txt.frq[1:min((maxrows+1), length(txt.frq))], sep = "\n")
        if( maxrows < uvals ) cat("... etc.\n [list output truncated]\n\n")  else cat("\n")
        lres$frq <- frq
      } else {
        # ... else give only highlow values    
        txt.highlow <- HighLow(x, na.rm=TRUE)
        cat(txt.highlow, "\n", sep="")
      }
    }
  }
  
  if(plotit) PlotDesc.integer(x, main=xname, maxrows=maxrows)

  invisible(lres)
  
}
