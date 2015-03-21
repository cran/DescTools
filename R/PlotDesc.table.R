PlotDesc.table <-
function(x, col1=getOption("col1", hblue), col2=getOption("col2", hred), 
                           horiz = TRUE, main=NA, ..., wrd=NULL){

  oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) ) 

  if(length(dim(x)) == 1){
    PlotDesc.factor(Untable(x)[,], main=main, ord="none", wrd=wrd)
    width <- 6
    height <- 4
    
  } else if(length(dim(x)) > 2){
    mosaicplot(x, main="", cex=0.8, las=1 # , xlab="", ylab=""
               , col=colorRampPalette(c(col1, "white", col2), space = "rgb")(ncol(x)), ... )
    
    width <- 8
    height <- 8  # dimension for 2 mosaicplots
    par(mfrow=c(1,1))
    par(mar=c(3.1,4.1,1.1,0.5), oma=c(0,0,ifelse(is.na(main), 0, 2),0))
    
  } else {
    
    if(horiz){
      width <- 16
      height <- 6.5  # dimension for 2 mosaicplots
      par(mfrow=c(1,2))
      par(mar=c(5.1,2.1,1.1,0.5), oma=c(0,0,ifelse(is.na(main), 0, 2),0))
    } else {  
      width <- 7
      height <- 14  # dimension for 2 mosaicplots
      par(mfrow=c(2,1))
      par(mar=c(3.1,4.1,1.1,0.5), oma=c(0,0,ifelse(is.na(main), 0, 2),0))
    }  
    
    mosaicplot(x, main="", cex=0.8, las=1 # , xlab="", ylab=""
      , col=colorRampPalette(c(col1, "white", col2), space = "rgb")(ncol(x)), ... )
  
    mosaicplot(t(x), main="", cex=0.8, las=1 # , xlab="", ylab=""
      , col=colorRampPalette(c(col1, "white", col2), space = "rgb")(ncol(t(x))), ... )
  
  }  

  if(!is.null(wrd)) WrdPlot(width=width, height=height, dfact=2.0, crop=c(0,0,0,0), wrd=wrd, append.cr=TRUE)
  
  if(!is.na(main)) title(main, outer=TRUE)
  
  # ToDo:
  # place another good table plot from vcd here, but which?....
  invisible()
  
}
