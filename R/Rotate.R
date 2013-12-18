Rotate <-
function( x, y, mx = 0, my = 0, theta=pi/3 ) {

  # which geom parameter has the highest dimension
  lgp <- list(x=x, y=y)
  maxdim <- max(unlist(lapply(lgp, length)))
  # recycle all params to maxdim
  lgp <- lapply( lgp, rep, length.out=maxdim )

  # rotate the structure 
  dx <- lgp$x - mx
  dy <- lgp$y - my
  ptx <- mx + cos(theta) * dx - sin(theta) * dy
  pty <- my + sin(theta) * dx + cos(theta) * dy

  return(list(x=ptx, y=pty))

}
