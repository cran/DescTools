DrawCircle <-
function( x = 0, y = x, radius = 1, rot = 0, nv = 100, border = par("fg"), col = par("bg")
  , lty = par("lty"), lwd = par("lwd"), plot = TRUE ) {
  invisible( DrawRegPolygon(  x = x, y = y, radius.x=radius, nv=nv, border=border, col=col, lty=lty, lwd=lwd, plot = plot ) )
}
