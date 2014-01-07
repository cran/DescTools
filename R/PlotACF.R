PlotACF <-
function(series, lag.max = 10*log10(length(series)), ...)  {
  
  ## Purpose:  time series plot with correlograms
  #  Original name: f.acf
  
  ## ---
  ## Arguments: series : time series
  ##           lag.max : the maximum number of lags for the correlograms
  
  
  ## ---
  ## Author: Markus Huerzeler, Date: 15 Jun 94
  ## Revision: Christian Keller, 5 May 98
  ## Revision: Markus Hürzeler, 11. März 04
  
  if (!is.null(dim(series)))
    stop("f.acf is only implemented for univariate time series")
  par(mfrow=c(1,1))
  old.par <- par(mar=c(3,3,1,1), mgp=c(1.5,0.5,0))
  on.exit(par(old.par))
  split.screen(figs=matrix(c(0,1,0.33,1, 0,0.5,0,0.33, 0.5,1,0,0.33),
                           ncol=4, byrow=T), erase=TRUE)
  ##screen(1)
  plot.ts(series, cex=0.7, ...)
  screen(2)
  PlotGACF(series, lag.max=lag.max, cex=0.7)
  screen(3)
  PlotGACF(series, lag.max=lag.max, type="partial", ylab="part. Autokorr", cex=0.7)
  close.screen(all.screens=TRUE)
  invisible(par(old.par))
}
