PlotDesc <-
function(x, ..., wrd=NULL){
  if(length(na.omit(x))==0) {

    plot(1, type="n", axes=FALSE, xlab="", ylab="", xlim=c(0,1),ylim=c(0,1), frame.plot=TRUE)
    text(x=0.5, y=0.5, labels="Nothing to plot...")
    
    if(!is.null(wrd)) WrdPlot(width=7.5, height=4.7, dfact=2.4, crop=c(0,0,0.7,0), wrd=wrd, append.cr=TRUE)
    
  } else {  
    if( is.vector(x) && length(unique(na.omit(x))) == 2) { 
      PlotDesc.logical(x=x, ..., wrd=wrd)
    } else {  
      UseMethod("PlotDesc") 
    }  
  }  
  
}
