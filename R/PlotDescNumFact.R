PlotDescNumFact <-
function( formula, data, main=deparse(formula), notch=FALSE,
  add_ni = TRUE, ... , wrd=NULL){

  # PlotMultiDens() would maybe be nice as well
  # or perhaps violinplot?? 
  
  # create a new graphics window
  usr <- par("usr");  on.exit( par(usr) ) 
  par( mar=c(5,4,2*add_ni,2)+.1, oma=c(0,0,4.1,0))
  
  layout( matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(2,1), TRUE)
  boxplot( formula, data, notch=notch, type="n", xaxt="n", yaxt="n", ... ) 
  grid(nx=NA, ny=NULL) 
  bx <- boxplot( formula, data, col="white", notch=notch, add=TRUE, cex.axis=0.8, ... )

  if(add_ni){ mtext( paste("n=", bx$n, sep=""), side=3, line=1, at=1:length(bx$n), cex=0.8) }
  
  plot.design(x=formula, data=data, cex=0.8, xlab="", ylab="", cex.axis=0.8, main="", ... )
  mtext( "means", side=3, line=1, cex=0.8)
  title(main=main, outer=TRUE)
  
  if(!is.null(wrd)) WrdPlot(width=15, height=7, dfact=2.2, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
  
}
