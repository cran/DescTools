Desc.integer <-
function(x, main = NULL, maxrows = 12, freq = NULL
  , digits = 3, plotit=getOption("plotit", FALSE), ... ) {


  if( is.null(main)) main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  
  uvals <- length(unique(na.omit(x)))
  
  if(is.null(freq)) freq <- uvals <= 12
  
  if(uvals == 2) {
    # if there are only 2 unique values, describe it as dichotomous variable
    lres <- Desc.logical(x, main=gsub(pattern="integer)", replacement="integer - dichotomous)", 
                                      x=main), plotit=FALSE) 

  } else {
    # produce the numeric summary
    lres <- Desc.numeric(x, main=main, highlow = FALSE, plotit = FALSE) 
    
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
  
  if(plotit) PlotDesc.integer(x, main=main, maxrows=maxrows)

  invisible(lres)
  
}
