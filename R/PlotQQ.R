PlotQQ <-
function(x, qdist, ..., main=NULL, xlab=NULL, ylab=NULL){

  # qqplot for an optional distribution

  # example:
  # y <- rexp(100, 1/10)
  # PlotQQ(y, "qexp", rate=1/10)
  # PlotQQ(y, qexp, rate=1/10)  # the function name can also be passed as function
  
  if(is.function(qdist)) { 
    # if qdist is a function, then save it under new name and 
    # overwrite function name in FUN, which has to be character
    fct <- qdist
    qdist <- "fct"
  } 
  
  y <- sort(x)
  x <- ppoints(y)
  x <- eval(parse(text=gettextf("%s(x, ...)", qdist)))
  
  if(is.null(main)) main <- gettextf("Q-Q-Plot", qdist)
  if(is.null(xlab)) xlab <- gettextf("Theoretical Quantiles (%s)", qdist)
  if(is.null(ylab)) ylab <- "Sample Quantiles"
  
  plot(x=x, y, main=main, xlab=xlab, ylab=ylab)
  abline(a=0, b=1)
}
