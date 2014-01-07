PlotDesc.table <-
function(x, col0=hred, col1=hblue, 
                           horiz = TRUE, main="", ..., wrd=NULL){

  oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) ) 

  if(horiz){
    width <- 16
    height <- 6.5  # dimension for 2 mosaicplots
    par(mfrow=c(1,2))
    par(mar=c(5.1,2.1,1.1,0.5), oma=c(0,0,ifelse(main=="", 0, 2),0))
  } else {  
    width <- 8
    height <- 14  # dimension for 2 mosaicplots
    par(mfrow=c(2,1))
    par(mar=c(3.1,4.1,1.1,0.5), oma=c(0,0,ifelse(main=="", 0, 2),0))
  }  
  
  mosaicplot(x, main="", cex=0.8, las=1, xlab="", ylab=""
    , col=colorRampPalette(c(col0, "white", col1), space = "rgb")(ncol(x)), ... )

  mosaicplot(t(x), main="", cex=0.8, las=1, xlab="", ylab=""
    , col=colorRampPalette(c(col0, "white", col1), space = "rgb")(ncol(t(x))), ... )

  if(!is.null(wrd)) WrdPlot(width=width, height=height, dfact=2.0, crop=c(0,0,0,0), wrd=wrd, append.cr=TRUE)
  
  title(main, outer=TRUE)
  
  # ToDo:
  # place another good table plot from vcd here, but which?....
  
}
