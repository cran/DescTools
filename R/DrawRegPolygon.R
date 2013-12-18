DrawRegPolygon <-
function( x = 0, y = x, radius.x = 1, radius.y = radius.x, rot = 0, nv = 3, 
      border = par("fg"), col = par("bg"), lty = par("lty"), lwd = par("lwd"), plot = TRUE ) {

    # The workhorse for the geom stuff
    
    # example:
    # plot(c(0,1),c(0,1), asp=1, type="n")
    # DrawRegPolygon( x=0.5, y=0.5, radius.x=seq(0.5,0.1,-0.1), rot=0, nv=3:10, col=2)
    # DrawRegPolygon( x=0.5+1:5*0.05, y=0.5, radius.x=seq(0.5,0.1,-0.1), rot=0, nv=100, col=1:5)

    # which geom parameter has the highest dimension
    lgp <- list(x=x, y=y, radius.x=radius.x, radius.y=radius.y, rot=rot, nv=nv)
    maxdim <- max(unlist(lapply(lgp, length)))
    # recycle all params to maxdim
    lgp <- lapply( lgp, rep, length.out=maxdim )
    
    # recycle shape properties
    if (length(col) < maxdim)    { col <- rep(col, length.out = maxdim) }
    if (length(border) < maxdim) { border <- rep(border, length.out = maxdim) }
    if (length(lwd) < maxdim)    { lwd <- rep(lwd, length.out = maxdim) }
    if (length(lty) < maxdim)    { lty <- rep(lty, length.out = maxdim) }

    lst <- list()   # prepare result
    for (i in 1:maxdim) {
        theta.inc <- 2 * pi / lgp$nv[i]
        theta <- seq(0, 2 * pi, by = theta.inc) 
        ptx <- cos(theta) * lgp$radius.x[i] + lgp$x[i]
        pty <- sin(theta) * lgp$radius.y[i] + lgp$y[i]
        if(lgp$rot[i] > 0){
          # rotate the structure if the angle is > 0
          dx <- ptx - lgp$x[i] 
          dy <- pty - lgp$y[i]
          ptx <- lgp$x[i] + cos(lgp$rot[i]) * dx - sin(lgp$rot[i]) * dy
          pty <- lgp$y[i] + sin(lgp$rot[i]) * dx + cos(lgp$rot[i]) * dy
        }
        if( plot ) 
          polygon(ptx, pty, border = border[i], col = col[i], lty = lty[i], 
              lwd = lwd[i])
        lst[[i]] <- list(x = ptx, y = pty)       
    }
    invisible(lst)
}
