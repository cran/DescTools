PlotDescNumNum <-
function( form1, form2, data, main = NULL, xlab= NULL, ylab= NULL, ... , wrd=NULL) {
  
  # create a new graphics window
  usr <- par("usr");  on.exit( par(usr) ) 
  par(mfrow=c(1,2), mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0))
  
  plot( form1, data=data, col=rgb(0,0,0,0.3), type="n", main=NA, xlab=xlab, ylab=ylab, ... )
  grid()
  points( form1, data=data, col=rgb(0,0,0,0.3) ) 
  lines(loess(form1, data=data))
  
  # und alles nochmals mit vertauschten Achsen
  # transponiere die formula
  plot( form2, data=data, col=rgb(0,0,0,0.3), type="n", main=NA, xlab=ylab, ylab=xlab,... )
  grid()
  points( form2, data=data, col=rgb(0,0,0,0.3) ) 
  lines(loess(form2, data=data))
  
  if(is.null(main)) main <- form1
  if(!is.na(main)) title(main = main, outer=TRUE, line = -1.5)

  if(!is.null(wrd)) WrdPlot(width=13, height=6.5, dfact=2.5, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
  invisible()
  
}
