PlotDescFactNum <-
function( x, y, ptab, col1=getOption("col1", hblue), col2=getOption("col2", hred), main=NULL, notch=FALSE,
                            add_ni = TRUE, ... , wrd=NULL){
  
  if(is.null(main)) main <- paste(deparse(substitute(y)), " ~ ", deparse(substitute(x)), sep="")

  usr <- par("usr");  on.exit( par(usr) ) 
  par( mar=c(5,4,2*add_ni,2)+.1, oma=c(0,0,4.1,0))
  
  layout( matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(1,2), TRUE)
  boxplot(x ~ y, notch=notch, type="n", xaxt="n", yaxt="n", ... ) 
  grid(nx=NA, ny=NULL) 
  bx <- boxplot( x ~ y, col="white", notch=notch, add=TRUE, cex.axis=0.8, ... )
  
  if(add_ni){ mtext( paste("n=", bx$n, sep=""), side=3, line=1, at=1:length(bx$n), cex=0.8) }
  
  if(nrow(ptab) < 3){
    plot(ptab[2,], xaxt="n", las=1, ylab="", xlab="Quantiles of x")
    axis(side=1, at=1:10, labels=gettextf("Q%s", 1:10))
    grid()
    if(ncol(ptab) > 6) { lines(loess(p ~ x, data.frame(p=ptab[2,], x=1:ncol(ptab)))) }
  } else {
    Mar(,,1,)
    mosaicplot(t(ptab), las=1, cex=1, col=colorRampPalette(c(col1, "white", col2), space = "rgb")(nrow(ptab)), main=NA)
  }
  
  title(main=main, outer=TRUE)
  
  if(!is.null(wrd)) WrdPlot(width=15, height=7, dfact=2.2, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
  invisible()
  
}
