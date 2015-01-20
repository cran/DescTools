PolarGrid <-
function(nr = NULL, ntheta = NULL, col = "lightgray"
  , lty = "dotted", lwd = par("lwd"), rlabels = NULL, alabels = NULL, lblradians = FALSE) {

  if (is.null(nr)) {             # use standard values with pretty axis values
      # at <- seq.int(0, par("xaxp")[2L], length.out = 1L + abs(par("xaxp")[3L]))
      at <- axTicks(1)[axTicks(1)>=0]
  } else if (!all(is.na(nr))) {  # use NA for suppress radial gridlines
    if (length(nr) > 1) {        # use nr as radius
      at <- nr 
    } else {
      at <- seq.int(0, par("xaxp")[2L], length.out = nr + 1)#[-c(1, nr + 1)]
    }
  } else {at <- NULL}
  if(!is.null(at)) DrawCircle(0, 0, at, border = col, lty = lty, col = NA)
  
  if (is.null(ntheta)) {             # use standard values with pretty axis values
      at.ang <- seq(0, 2*pi, by=2*pi/12) 
  } else if (!all(is.na(ntheta))) {  # use NA for suppress radial gridlines
    if (length(ntheta) > 1) {        # use ntheta as angles
      at.ang <- ntheta 
    } else {
      at.ang <- seq(0, 2*pi, by=2*pi/ntheta) 
    }
  } else {at.ang <- NULL}
  if(!is.null(at.ang)) segments(x0=0, y0=0, x1=max(par("usr"))*cos(at.ang)
    , y1=max(par("usr"))*sin(at.ang), col = col, lty = lty, lwd = lwd)

  # plot radius labels
  if(!is.null(at)){
    if(is.null(rlabels)) rlabels <- signif(at[-1],3)   # standard values
    if(!all(is.na(rlabels))) BoxedText( x=at[-1], y=0, labels=rlabels, border=FALSE, col="white")    
  } 

  # plot angle labels
  if(!is.null(at.ang)){
    if(is.null(alabels)) 
      if( lblradians == FALSE ){
        alabels <- RadToDeg(at.ang[-length(at.ang)])   # standard values in degrees
      } else {
        alabels <- Format(at.ang[-length(at.ang)], digits=2)   # standard values in radians
      }    
    if(!all(is.na(alabels))) 
      BoxedText( x=par("usr")[2]*1.07*cos(at.ang)[-length(at.ang)], y=par("usr")[2]*1.07*sin(at.ang)[-length(at.ang)]
        , labels=alabels, border=FALSE, col="white")
  } 

}
