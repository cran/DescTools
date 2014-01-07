DrawAnnulus <-
function (x = 0, y = x, radius.in = 1, radius.out = 2, nv = 100, border = par("fg")
  , col = par("bg"), lty = par("lty"), lwd = par("lwd"), plot = TRUE) {
    
  pts.out <- DrawCircle(x = x, y = y, radius = radius.out, plot = FALSE)
  pts.in <- DrawCircle(x = x, y = y, radius = radius.in, plot = FALSE)

  ptx <- c( unlist(lapply(pts.out, "[", "x")), rev(unlist(lapply(pts.in, "[", "x"))) )
  pty <- c( unlist(lapply(pts.out, "[", "y")), rev(unlist(lapply(pts.in, "[", "y"))) )

  # we have to use polygon here, because of the transparent hole in the middle..
  # but don't know how to ged rid of the closing line, so draw polygon without border and then redraw circles
  polygon(x = ptx, y = pty, col = col, border = NA, lty = lty, lwd = lwd)
  lapply( pts.out, lines, col=border, lty=lty, lwd=lwd )
  lapply( pts.in, lines, col=border, lty=lty, lwd=lwd )

  invisible(list(x = ptx, y = pty))
  
}
