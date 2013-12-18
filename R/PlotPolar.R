PlotPolar <-
function(r, theta = NULL, type="p"
  , rlim = NULL, main="", lwd = par("lwd"), lty = par("lty"), col = par("col")
  , pch = par("pch"), fill = NA, cex = par("cex")
  , mar = c(2, 2, 5, 2), add = FALSE, ...) {

  
  if( ncol(r <- as.matrix(r)) == 1) r <- t(r)
  k <- nrow(r)
  
  if(is.null(theta)) {
    theta <- seq(0, 2*pi, length=ncol(r)+1)[-(ncol(r)+1)]
    if( nrow(r) > 1 ){
      theta <- matrix( rep(theta, times=nrow(r)), ncol=ncol(r), byrow = TRUE )
    }  else {
      theta <- t(as.matrix(theta))
    }  
  } else {
    if( ncol(theta <- as.matrix(theta)) == 1) theta <- t(theta)
  }
  
  # kang <- ncol(theta <- as.matrix(theta))
  # n <- nrow(r)
  # ### if (n != nrow(theta)) 
      # ### stop("'r' and 'theta' must have same number of rows")
  # if (kr > 1 && kang > 1 && kr != kang) 
      # stop("'r' and 'theta' must have only 1 or the same number of columns")
  # if (kr == 1) 
      # r <- matrix(r, nrow = n, ncol = kang)
  # if (kang == 1) 
      # theta <- matrix(theta, nrow = n, ncol = kr)

  
  if (length(type) < k) type <- rep(type, length.out = k)
  if (length(lty) < k)  lty <- rep(lty, length.out = k)
  if (length(lwd) < k)  lwd <- rep(lwd, length.out = k)
  if (length(pch) < k)  pch <- rep(pch, length.out = k)
  if (length(col) < k)  col <- rep(col, length.out = k)
  if (length(fill) < k) fill <- rep(fill, length.out = k)
  if (length(cex) < k)  cex <- rep(cex, length.out = k)

  dev.hold()
  on.exit(dev.flush())

  # definition follows plot.default()
  rlim <- if (is.null(rlim)) max(abs(r[is.finite(r)]))*1.12
  if(!add){
    par(mar = mar, pty = "s", xpd=TRUE)
    plot(x=c(-rlim, rlim), y=c(-rlim, rlim), 
      type = "n", axes = FALSE, main = main, xlab = "", ylab = "", ...)
  }

  for (i in seq_len(k)) {
    xy <- xy.coords( x=cos(theta[i,]) * r[i,], y=sin(theta[i,])*r[i,])
    if(type[i] == "p"){
      points( xy, pch = pch[i], col = col[i], cex = cex[i] )
    } else if( type[i]=="l") {
      polygon(xy, lwd = lwd[i], lty = lty[i], border = col[i], col = fill[i])
    } else if( type[i]=="h") {
      segments(x0=0, y0=0, x1=xy$x, y1=xy$y, lwd = lwd[i], lty = lty[i], col = col[i])
    }  
  }
}
