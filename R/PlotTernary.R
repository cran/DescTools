PlotTernary <-
function(x, y = NULL, z = NULL, args.grid=NULL, lbl = NULL, main = "", ...){
  
  
  if(!(is.null(y) && is.null(z))){
    if(is.null(lbl)) lbl <- c(names(x), names(y), names(z))
    x <- cbind(x, y, z)
  } else {
    if(is.null(lbl)) lbl <- colnames(x)
    x <- as.matrix(x)
  }
  
  if(any(x < 0)) stop("X must be non-negative")
  s <- drop(x %*% rep(1, ncol(x)))
  if(any(s<=0)) stop("each row of X must have a positive sum")
  if(max(abs(s-1)) > 1e-6) {
    warning("row(s) of X will be rescaled")
    x <- x / s
  }
    
  oldpar <- par(xpd=TRUE)
  on.exit(par(oldpar))
  Canvas(mar=c(1,3,4,1) + .1, main=main)
  
  sq3 <- sqrt(3)/2
  
  # grid: define default arguments 
  if(!identical(args.grid, NA)){
    args.grid1 <- list(col="grey", lty="dotted", nx=5) 
    # override default arguments with user defined ones
    if (!is.null(args.grid)) { 
      args.grid1[names(args.grid)] <- args.grid 
    } 
    
    d <- seq(0, 2*sq3, sq3*2/(args.grid1$nx))
    x0 <- -sq3 + (1) * d
    segments(x0 = x0, y0 = -0.5, x1 = x0 + sq3 - d*.5, y1 = 1- d * sq3, col=args.grid1$col, lty=args.grid1$lty)
    segments(x0 = x0, y0 = -0.5, x1 = -rev(x0 + sq3 - d*.5), y1 = rev(1- d * sq3), col=args.grid1$col, lty=args.grid1$lty)
    segments(x0 = x0 + sq3 - d*.5, y0 = 1- d * sq3, x1 = rev(x0 -d*.5), y1 = 1- d * sq3, col=args.grid1$col, lty=args.grid1$lty)
  }
  
  DrawRegPolygon(nv = 3, rot = pi/2, radius.x = 1, col=NA)
  
  eps <-0.15
  pts <- DrawRegPolygon(nv = 3, rot = pi/2, radius.x = 1+eps, plot=FALSE)[[1]]
  
  text(pts, labels = lbl[c(1,3,2)])
  
  points((x[,2] - x[,3]) * sq3, x[,1] * 1.5 - 0.5, ...)
  
}
