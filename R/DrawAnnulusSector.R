DrawAnnulusSector <-
function (x = 0, y = x, radius.in = 1, radius.out = 2, angle.beg = 0, angle.end = pi
  , nv = 100, border = par("fg"), col = par("bg"), lty = par("lty"), lwd = par("lwd"), plot = TRUE) {

  DrawSector <- function(x, y, radius.in, radius.out, angle.beg, angle.end
      , nv, border, col, lty, lwd, plot) {
    # let DrawArc calculate the 2 arcs
    pts <- DrawArc( x=x, y=y, radius.x = c(radius.out, radius.in), radius.y = c(radius.out, radius.in)
      , angle.beg = angle.beg, angle.end = angle.end, nv = nv
      , col = border, lty = lty, lwd = lwd, plot = FALSE )
    # combine the arcs to a annulus sector
    ptx <- c(pts[[1]]$x, rev(pts[[2]]$x))
    pty <- c(pts[[1]]$y, rev(pts[[2]]$y))  
    if( plot ) { polygon(x = ptx, y = pty, col = col, border = border, lty = lty, lwd = lwd) }
    invisible(list(x = ptx, y = pty))
  }
  
  # which geom parameter has the highest dimension
  lgp <- list(x = x, y = y, radius.in = radius.in, radius.out = radius.out, 
      angle.beg = angle.beg, angle.end = angle.end, nv = nv)
  maxdim <- max(unlist(lapply(lgp, length)))
  # recycle all params to maxdim
  lgp <- lapply(lgp, rep, length.out = maxdim)

  # recycle shape properties
  if (length(col) < maxdim) { col <- rep(col, length.out = maxdim) }
  if (length(border) < maxdim) { border <- rep(border, length.out = maxdim) }
  if (length(lwd) < maxdim) { lwd <- rep(lwd, length.out = maxdim) }
  if (length(lty) < maxdim) { lty <- rep(lty, length.out = maxdim) }

  # Draw the single sectors
  lst <- list()
  for (i in 1:maxdim) {
    pts <- DrawSector( x = lgp$x[i], y = lgp$y[i], radius.in = lgp$radius.in[i], radius.out = lgp$radius.out[i]
      , angle.beg = lgp$angle.beg[i], angle.end = lgp$angle.end[i], nv = lgp$nv[i]
      , border = border[i], col = col[i], lty = lty[i], lwd = lwd[i], plot = plot )
    lst[[i]] <- pts
  }
  invisible(lst)
  
}
