ConnLines <-
function(..., col = 1, lwd = 1, lty = "solid", xalign = c("mar","mid") ) {

  # add connection lines to a barplot
  # ... are the arguments, passed to barplot
  
  b <- barplot(..., plot = FALSE) 
  
  arg <- unlist(match.call(expand.dots = FALSE)$...)
  if(is.null(arg$horiz)) horiz <- FALSE else horiz <- eval(arg$horiz, parent.frame())
  # debug: print(horiz)
  
  nr <- nrow(eval(arg[[1]], parent.frame())) # nrow(height) 
  nc <- length(b)

  if(!is.null(nr)) {
    tmpcum <- apply(eval(arg[[1]], parent.frame()), 2, cumsum) 
    ypos1 <- tmpcum[, -nc] 
    ypos2 <- tmpcum[, -1] 

  } else {
    tmpcum <- eval(arg[[1]], parent.frame())
    ypos1 <- tmpcum[-nc] 
    ypos2 <- tmpcum[-1] 
    nr <- 1
  }  

  xalign <- match.arg(xalign)
  if(xalign=="mar"){
    
    # the midpoints of the bars
    mx <- (b[-1] + b[-length(b)]) / 2 
  
    if(is.null(arg$space)) space <- 0.2
    else space <- eval(arg$space, parent.frame())
    
    lx <- mx - space/2 
    rx <- mx + space/2 
    
    xpos1 <- rep(lx, rep(nr, length(lx))) 
    xpos2 <- rep(rx, rep(nr, length(rx))) 
    
    if(horiz == FALSE)
      segments(xpos1, ypos1, xpos2, ypos2, col=col, lwd=lwd, lty=lty) 
    else
      segments(ypos1, xpos1, ypos2, xpos2, col=col, lwd=lwd, lty=lty) 
  
  } else if(xalign=="mid") {
    if(horiz == FALSE) {    
      if(nr > 1)
        matlines(x=replicate(nr, b), y=t(tmpcum), lty=lty, lwd=lwd, col=col)
      else 
        lines(x=b, y=tmpcum, lty=lty, lwd=lwd, col=col)
    } else {
      if(nr > 1)
        matlines(y=replicate(nr, b), x=t(tmpcum), lty=lty, lwd=lwd, col=col)
      else 
        lines(y=b, x=tmpcum, lty=lty, lwd=lwd, col=col)
      
    }    
  }
  
  invisible()  
  
}
