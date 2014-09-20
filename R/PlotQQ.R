PlotQQ <-
function(x, qdist, main=NULL, xlab=NULL, ylab=NULL, args.qqline=NULL, ...){

  # qqplot for an optional distribution

  # example:
  # y <- rexp(100, 1/10)
  # PlotQQ(y, function(p) qexp(p, rate=1/10))
  
  y <- sort(x)
  x <- ppoints(y)
  x <- qdist(x)
  
  if(is.null(main)) main <- gettextf("Q-Q-Plot", qdist)
  if(is.null(xlab)) xlab <- "Theoretical Quantiles"
  if(is.null(ylab)) ylab <- "Sample Quantiles"
  
  plot(x=x, y, main=main, xlab=xlab, ylab=ylab)

  # add qqline if desired
  add.qqline <- TRUE
  if(!is.null(args.qqline)) if(all(is.na(args.qqline))) {add.qqline <- FALSE} 
  
  if(add.qqline) {

    # define default arguments for ci.band
    args.qqline1 <- list(probs = c(0.25, 0.75), qtype=7, col=par("fg"), lwd=par("lwd"), lty=par("lty")) 
    # override default arguments with user defined ones
    if (!is.null(args.qqline)) args.qqline1[names(args.qqline)] <- args.qqline 
    
    # estimate qqline, instead of set it to abline(a = 0, b = 1)
    # plot qqline through the 25% and 75% quantiles (same as qqline does for normal dist) 
    ly <- quantile(y, prob=args.qqline1[["probs"]], type=args.qqline1[["qtype"]], na.rm = TRUE)
    lx <- qdist(args.qqline1[["probs"]])
    
    slope <- diff(ly) / diff(lx)
    int <- ly[1L] - slope * lx[1L]
    do.call("abline", c(args.qqline1[c("col","lwd","lty")], list(a=int, b=slope)) ) 
    
  }
  
}
