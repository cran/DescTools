AddErrBars <-
function(from, to = NULL, pos = NULL, mid = NULL, horiz = FALSE, col = par("fg"), lty = par("lty"),
                       lwd = par("lwd"), code = 3, length=0.05, 
                       pch = NA, cex.pch = par("cex"), col.pch = par("fg"), bg.pch = par("bg") ) {

  if(is.null(to)) {
    if(!any(dim(from) == 2)) stop("x0 must be a kx2 matrix, when x1 is not provided.")
    if(dim(from)[1] == 2) {
      to <- from[2,]
      from <- from[1,]
    } else {
      to <- from[,2]
      from <- from[,1]
    }  
  }  
  if(is.null(pos)) pos <- 1:length(from)
  if(horiz){
    arrows( x0=from, x1=to, y0=pos, col=col, lty=lty, lwd=lwd, angle=90, code=code, length=length )
  } else {
    arrows( x0=pos, y0=from, y1=to, col=col, lty=lty, lwd=lwd, angle=90, code=code, length=length )
  }
  if(!is.na(pch)){
    if(is.null(mid)) mid <- (from + to)/2
    # plot points
    if(horiz){
      points(x=mid, y=pos, pch = pch, cex = cex.pch, col = col.pch, bg=bg.pch)
    } else {
      points(x=pos, y=mid, pch = pch, cex = cex.pch, col = col.pch, bg=bg.pch)
    }  
  }
}
