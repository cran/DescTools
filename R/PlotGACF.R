PlotGACF <-
function(series, lag.max=10*log10(length(series)), type="corr", ylab="Auto-Korr.", ...)
{
  ## Author: Markus Huerzeler, Date:  6 Jun 94
  ## Revision: Christian Keller, 27 Nov 98
  ## Revision: Markus Hürzeler, 11 Mar 02
  
  # original name g.plot.acf
  # erg <- acf(series, type=type, plot=FALSE, lag.max=lag.max, na.action=na.omit)
  erg <- acf(na.omit(series), type=type, plot=FALSE, lag.max=lag.max)
  
  erg.acf <- erg$acf
  erg.konf <- 2/sqrt(erg$n.used)
  yli <- range(c(erg.acf, erg.konf, -erg.konf))*c(1.1, 1.1)
  erg.lag <- as.vector(erg$lag)
  
  ## Labels fuer x-Achse definieren:
  ## 1. Label ist immer erg.lag[1]
  pos <- pretty(erg.lag)
  n <- length(pos)
  d <- pos[2] - pos[1] ; f <- pos[1]-erg.lag[1]
  pos <- c(erg.lag[1], pos[1][f > d/2], pos[2:n])
  
  plot(erg.lag, erg.acf, type="h", ylim=yli, xlab="Lag k", ylab=ylab,
       xaxt="n", ...)
  axis(1, at=pos, ...)
  abline(0,0)
  abline(h=c(erg.konf, - erg.konf), lty=2)
  invisible()
}
