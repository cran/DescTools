Desc.numeric <-
function(x, xname=NULL
  , highlow = TRUE, plotit = FALSE, ... ) {  

  # Kennzahlen für die Beschreibung von kardinalen Variablen
  # na.rm = TRUE für die Kennzahlen, NAs werden ausgewiesen
  
  if( !class(x) %in% c("numeric","integer") ) stop( gettextf("!Desc.numeric! cannot handle class %s", class(x)))
  if( is.null(xname)) xname <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), sep=", "))

  # get NAs and forget them
  n <- length(x)
  NAs <- sum(is.na(x))
  x <- na.omit(x)

  meanx <- mean(x)
  sdx <- sd(x)
  
  # get the names, numerical results and organize in list
  quant <- quantile(x, c(.05,.10,.25,.5,.75,.9,.95))
  lres <- list( 
      l1 = list( length=n, n=n-NAs, NAs=NAs, unique=length(unique(x))
             , `0s`=sum(x==0)
             , mean=meanx, meanSE=sdx / sqrt(n-NAs) )
    , l2 = as.list(quant)
    , l3 = list ( rng=diff(range(x)), sd=sdx, vcoef=sdx/meanx
             , mad=mad(x)
             , IQR=IQR(x), skew=Skew(x), kurt=Kurt(x))
  )
  # change names for quantiles
  names(lres[[2]]) <- c(".05",".10",".25","median",".75",".90",".95")

  cat( paste(rep("-", (as.numeric(options("width"))-2)), collapse=""), "\n" ) 
  if(!is.na(xname)) cat( xname )
  if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent = 2), sep="\n" )
  cat("\n\n") 
  
  # format values according to defined pretty nums
  lfmt <- lapply( lres, lapply, .fmt )        
  # what's max width in names and formatted values?
  width <- max( c( unlist(lapply(lfmt, nchar)), unlist(lapply( lapply(lfmt, names), nchar))))  
  
  cat( paste(lapply(lfmt, .txtline, width=width, ind="  ", space=" "), collapse="\n" ), "\n")
    
  txt.highlow <- HighLow(x, na.rm=TRUE)
  if( highlow ){  # highlow is not always interesting in output ...
    cat(txt.highlow, "\n", sep="")
  }
  
  if(lres$l1$n %[]% c(5,5000)) {
    res <- tryCatch(shapiro.test(x), error=function(e) {e})  
    
    if (inherits(res, "simpleError")) {
      cat(gettextf("Error in shapiro.test(x) : %s\n\n", res$message))  
    } else {
      cat(gettextf("Shapiro-Wilks normality test  p.value : %s", 
                   format.pval(res$p.value)), "\n\n")
    }
  }
  if(lres$l1$n > 5000) {
    res <- tryCatch(AndersonDarlingTest(x), error=function(e) {e})  
    
    if (inherits(res, "simpleError")) {
      cat(gettextf("Error in AndersonDarlingTest(x) : %s\n\n", res$message))  
    } else {
      cat(gettextf("Anderson-Darling normality test  p.value : %s", 
                   format.pval(res$p.value)), "\n\n")
    }
    
  }
  
#  if(plotit) PlotDesc.numeric(x, newwin = TRUE), but only if there are valid points to plot
  if(plotit) if(lres$l1$n > 0) PlotDesc.numeric(x, main=xname)
  
  invisible( c( lres$l1[], quant=list(quant), lres$l3[], highlow=txt.highlow) )
  
}
