PlotDescNumNum <-
function( form1, form2, data, ... , wrd=NULL) {
  
  # create a new graphics window
  usr <- par("usr");  on.exit( par(usr) ) 
  par(mfrow=c(1,2), mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0))
  
  plot( form1, data=data, col=rgb(0,0,0,0.3), type="n", ... )
  grid()
  points( form1, data=data, col=rgb(0,0,0,0.3) ) 
  AddLoess(form1, data=data)
  
  # und alles nochmals mit vertauschten Achsen
  # transponiere die formula
  plot( form2, data=data, col=rgb(0,0,0,0.3), type="n", ... )
  grid()
  points( form2, data=data, col=rgb(0,0,0,0.3) ) 
  AddLoess(form2, data=data)
  
  if(!is.null(wrd)) WrdPlot(width=13, height=6.5, dfact=2.5, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
  
}
