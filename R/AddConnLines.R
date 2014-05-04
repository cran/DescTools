AddConnLines <-
function(..., lcol = 1, lwd = 1, lty = "solid" ) {

  # add connection lines to a barplot
  # ... are the arguments, passed to barplot
  
  b <- barplot(..., plot = FALSE) 
  
  arg <- unlist(match.call(expand.dots = FALSE)$...)
  if(is.null(arg$horiz)) horiz <- FALSE else horiz <- eval(arg$horiz, parent.frame())
  print(horiz)
  
  # the midpoints of the bars
  mx <- (b[-1]+b[-length(b)])/2 
  if(is.null(arg$space)) 
    if(horiz == TRUE) space <- 1 else space = 0.2
  else space <- eval(arg$space, parent.frame())
  
  lx <- mx - space/2 
  rx <- mx + space/2 
  
  nr <- nrow(eval(arg[[1]], parent.frame())) # nrow(height) 
  nc <- length(b)
  
  xpos1 <- rep(lx, rep(nr, length(lx))) 
  xpos2 <- rep(rx, rep(nr, length(rx))) 
  
  tmpcum <- apply(eval(arg[[1]], parent.frame()), 2, cumsum) 
  ypos1 <- tmpcum[, -nc] 
  ypos2 <- tmpcum[, -1] 
  
  if(horiz == FALSE)
    segments(xpos1, ypos1, xpos2, ypos2, col=lcol, lwd=lwd, lty=lty) 
  else
    segments(ypos1, xpos1, ypos2, xpos2, col=lcol, lwd=lwd, lty=lty) 
  
  
}
