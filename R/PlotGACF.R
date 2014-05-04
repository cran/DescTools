PlotGACF <-
function(series, lag.max=10*log10(length(series)), type="cor", ylab=NULL, ...)
{
  ## Author: Markus Huerzeler, Date:  6 Jun 94
  ## Revision: Christian Keller, 27 Nov 98
  ## Revision: Markus Hürzeler, 11 Mar 02
  ## Correction for axis labels with ts-objects and deletion of ACF(0), Andri/10.01.2014
  
  # original name g.plot.acf
  # erg <- acf(series, type=type, plot=FALSE, lag.max=lag.max, na.action=na.omit)
  
  # debug:  series <- AirPassengers
  type <- match.arg(type, c("cor","cov","part"))
  
  erg <- acf(na.omit(series), type=type, plot=FALSE, lag.max=lag.max)
    
  erg.acf <- erg$acf
  # set the first acf(0) = 1 to 0
  if(type=="cor") {
    erg.acf[1] <- 0
    if(is.null(ylab)) ylab <- "ACF"
  }  
  if(type=="part") {
    # add a 0-value to the partial corr. fct. 
    erg.acf <- c(0, erg.acf)
    if(is.null(ylab)) ylab <- "PACF"
  }  
  
  erg.konf <- 2/sqrt(erg$n.used)
  yli <- range(c(erg.acf, erg.konf, -erg.konf))*c(1.1, 1.1)
  # old: erg.lag <- as.vector(erg$lag)
  # new: get rid of the phases and use lags even with timeseries
  erg.lag <- seq_along(erg.acf)-1
  
  ## Labels fuer x-Achse definieren:
  ## 1. Label ist immer erg.lag[1]
  pos <- pretty(c(0, erg.lag))
  n <- length(pos)
  d <- pos[2] - pos[1] ; f <- pos[1]-erg.lag[1]
  pos <- c(erg.lag[1], pos[1][f > d/2], pos[2:n])
  
  plot(erg.lag, erg.acf, type="h", ylim=yli, xlab="Lag k", ylab=ylab,
       xaxt="n", xlim=c(0,length(erg.acf)), ...)
  axis(1, at=pos, ...)
  abline(0,0)
  abline(h=c(erg.konf, - erg.konf), lty=2, col="blue")
  invisible()
}
