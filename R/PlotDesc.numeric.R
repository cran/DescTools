PlotDesc.numeric <-
function(x, main = deparse(substitute(x)), ..., wrd=NULL) { 
  # just pass everything to PlotFdist, but omit NAs as density in PlotFDist would not handle it..
  PlotFdist(x=na.omit(x), main=main, ...)
  if(!is.null(wrd)) WrdPlot(width=8, height=5, dfact=2.1, crop=c(0,0,0.2,0), wrd=wrd, append.cr=FALSE)
}
