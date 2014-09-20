Desc.numeric <-
function(x, main = NULL
  , highlow = TRUE, plotit=getOption("plotit", FALSE), ... ) {  

  # unique and HighLow (sort) are expensive!
  
  # Kennzahlen für die Beschreibung von kardinalen Variablen
  # na.rm = TRUE für die Kennzahlen, NAs werden ausgewiesen
  
  if( !class(x) %in% c("numeric","integer") ) stop( gettextf("x must be numeric and has class %s", class(x)))
  if( is.null(main)) main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  
  # store label attribute, as na.omit will kill it!!
  lbl <- attr(x,"label")
  
  # get NAs and forget them
  n <- length(x)
  NAs <- sum(is.na(x))
  x <- na.omit(x)

  meanx <- mean(x)
  sdx <- sd(x)
  
  # get the names, numerical results and organize in list
  quant <- quantile(x, c(0, .05,.10,.25,.5,.75,.9,.95, 1))
  lres <- list( 
      l1 = list( length=n, n=n-NAs, NAs=NAs, unique=length(unique(x))
             , `0s`=sum(x==0)
             , mean=meanx, meanSE=sdx / sqrt(n-NAs) )
    , l2 = as.list(quant[2:8])
    , l3 = list ( rng=diff(quant[c(1,9)]), sd=sdx, vcoef=sdx/meanx
             , mad=mad(x)
             , IQR=diff(quant[c(4,6)]), skew=Skew(x), kurt=Kurt(x))
  )
  # change names for quantiles
  names(lres[[2]]) <- c(".05",".10",".25","median",".75",".90",".95")

  cat( paste(rep("-", (as.numeric(options("width"))-2)), collapse=""), "\n" ) 
  if(!identical(main, NA)) cat( main )
  if( !is.null(lbl) ) cat(" :", strwrap(lbl, indent=2, exdent = 2), sep="\n" )
  cat("\n\n") 
  
  # format values according to defined pretty nums
  lfmt <- lapply( lres, lapply, .fmt )        
  # what's max width in names and formatted values?
  width <- max( c( unlist(lapply(lfmt, nchar)), unlist(lapply( lapply(lfmt, names), nchar))))  
  
  # change unique value if ==n, as with big vectors it will not be clear if all unique or not
  if(lfmt$l1$unique == lfmt$l1$n) lfmt$l1$unique <- "= n"
  
  cat( paste(lapply(lfmt, .txtline, width=width, ind="  ", space=" "), collapse="\n" ), "\n")
    
  if( highlow ){  # highlow is not always interesting in output ...
    txt.highlow <- HighLow(x, na.rm=TRUE)
    cat(txt.highlow, "\n", sep="")
  } else {
    txt.highlow <- NA
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
    res <- tryCatch(AndersonDarlingTest(x, null="pnorm"), error=function(e) {e})  
    
    if (inherits(res, "simpleError")) {
      cat(gettextf("Error in AndersonDarlingTest(x, null='pnorm') : %s\n\n", res$message))  
    } else {
      cat(gettextf("Anderson-Darling normality test  p.value : %s", 
                   format.pval(res$p.value)), "\n\n")
    }
    
  }
  
#  if(plotit) PlotDesc.numeric(x, newwin = TRUE), but only if there are valid points to plot
  if(plotit) if(lres$l1$n > 0) PlotDesc.numeric(x, main=main)
  
  invisible( c( lres$l1[], quant=list(quant), lres$l3[], highlow=txt.highlow) )
  
}
