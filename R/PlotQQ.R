PlotQQ <-
function(x, qdist, main=NULL, xlab=NULL, ylab=NULL, args.qqline=NULL,  ...){

  # qqplot for an optional distribution

  # example:
  # y <- rexp(100, 1/10)
  # PlotQQ(y, function(p) qexp(p, rate=1/10))
  
  y <- sort(x)
  p <- ppoints(y)
  x <- qdist(p)
  
  if(is.null(main)) main <- gettextf("Q-Q-Plot", qdist)
  if(is.null(xlab)) xlab <- "Theoretical Quantiles"
  if(is.null(ylab)) ylab <- "Sample Quantiles"
  
  plot(x=x, y, main=main, xlab=xlab, ylab=ylab, ...)

  
# John Fox implements a envelope option in car::qqplot, in the sense of:
#   (unfortunately using ddist...)
# 
#   # add qqline if desired
#   if(!identical(args.band, NA)) {
#     n <- length(x)
#     zz <- qnorm(1 - (1 - args.band$conf.level) / 2)
#     SE <- (slope / d.function(z, ...)) * sqrt(p * (1 - p) / n)
#     fit.value <- int + slope * z
#     
#     upper <- fit.value + zz * SE
#     lower <- fit.value - zz * SE
#     
#     lines(z, upper, lty = 2, lwd = lwd, col = col.lines)
#     lines(z, lower, lty = 2, lwd = lwd, col = col.lines)
#   }

  # add qqline if desired
  if(!identical(args.qqline, NA)) {

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
