PlotMultiDens.default <-
function( x, xlim = NULL, ylim = NULL
                                   , col = rainbow(length(x)), lty = "solid", lwd = 1
                                   , xlab = "x", ylab = "density"
                                   , args.dens = NULL
                                   , args.legend = NULL
                                   , na.rm = FALSE, flipxy=FALSE, ...) {
  
  # the input MUST be a numeric list, use split if there's no list:
  #   PlotMultiDens(list(x,y,z))
  
  # Alternative:
  # library(lattice)
  # densityplot(  ~ vl|  vjdeck + region_x, data=d.set )
  
  FlipDensXY <- function(x){
    # flips x and y values of a density-object
    tmp <- x$x
    x$x <- x$y
    x$y <- tmp
    return(x)
  }
  
  # na.omit if wished 
  if(na.rm) x <- lapply(x, na.omit)
  
  args.dens1 <- list(n = 2^12, kernel="epanechnikov")     # default values
  if (!is.null(args.dens)) {
    args.dens1[names(args.dens)] <- args.dens
  }
  
  # recycle density arguments
  maxdim <- max(length(x), unlist(lapply(args.dens1, length)))
  args.dens1 <- lapply( args.dens1, rep, length.out=maxdim )
  
  # recycle x
  x <- rep(x, length.out=maxdim )
  
  # let's calculate the densities
  l.dens <- list()
  for(i in 1:maxdim)  {
    if(length(x[[i]]) > 2)    
      l.dens[[i]] <- if(flipxy) { 
        # FlipDensXY(density(x[[i]], n=n[i])) 
        FlipDensXY(do.call("density", append(list(x[[i]]), lapply(args.dens1,"[", i)) ))
      } else { 
        do.call("density", append(list(x[[i]]), lapply(args.dens1,"[", i)) )
      }
  }
  
  # recycle line attributes
  # which geom parameter has the highest dimension
  l.par <- list(lty=lty, lwd=lwd, col=col)
  l.par <- lapply( l.par, rep, length.out = maxdim )
  
  if( missing("xlim") ) xlim <- range(pretty( unlist(lapply(l.dens, "[", "x")) ) )
  if( missing("ylim") ) ylim <- range(pretty( unlist(lapply(l.dens, "[", "y")) )) 
  
  plot( x=1, y=1, xlim = xlim, ylim = ylim, type="n", xlab=xlab, ylab=ylab, ... )
  for(i in 1:length(l.dens))  {
    lines( l.dens[[i]], col=l.par$col[i], lty=l.par$lty[i], lwd=l.par$lwd[i] )
  }
  
  
  args.legend1 <- list( x="topright", inset=0.02, legend=if(is.null(names(x))){1:length(x)} else {names(x)}
                        , fill=col, bg="white", cex=0.8 )
  if( length(unique(lwd))>1 || length(unique(lty))>1 ) {
    args.legend1[["fill"]] <-  NULL
    args.legend1[["col"]] <- col
    args.legend1[["lwd"]] <- lwd
    args.legend1[["lty"]] <- lty
  }  
  if ( !is.null(args.legend) ) { args.legend1[names(args.legend)] <- args.legend }
  add.legend <- TRUE
  if(!is.null(args.legend)) if(all(is.na(args.legend))) {add.legend <- FALSE} 
  
  if(add.legend) do.call("legend", args.legend1)
  
  res <- do.call(rbind, lapply((lapply(l.dens, "[", c("bw","n"))), data.frame))
  res$kernel <- unlist(args.dens1["kernel"])

  invisible(res)
  
}
