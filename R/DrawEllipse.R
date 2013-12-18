DrawEllipse <-
function( x = 0, y = x, radius.x = 1, radius.y = 0.5, rot = 0, nv = 100, border = par("fg"), col = par("bg")
  , lty = par("lty"), lwd = par("lwd"), plot = TRUE ) {
  invisible( DrawRegPolygon(  x = x, y = y, radius.x = radius.x, radius.y = radius.y, nv = nv, rot = rot
    , border = border, col = col, lty = lty, lwd = lwd, plot = plot ) )
}
