PlotDotCIp <-
function(x, n, xlim=c(0,1), ord=c("rel", "abs", "names"), decreasing=FALSE, ... ) {

  # example: 
  
  # tab <- table( d.pizza$driver, d.pizza$wine_delivered)
  # PlotDotCIp( x=tab[,2], n=apply(tab,1,sum), ord="abs", dec=TRUE )

  # parameter recycling for x and n:
  #   which parameter has the highest dimension
  lgp <- list(x=x, n=n)
  maxdim <- max(unlist(lapply(lgp, length)))
  #   recycle all params to maxdim
  lgp <- lapply( lgp, rep, length.out=maxdim )
  
  ci <- t(sapply(seq_along(x), function(i) BinomCI( x=lgp$x[i], n=lgp$n[i]) ))
  rownames(ci) <- names(x)
  switch( match.arg( arg=ord, choices=c("rel", "abs", "names") )
    , "rel" = { idx <- order(ci[,1], decreasing=decreasing) }
    , "abs" = { idx <- order(ci[,2], decreasing=decreasing) }
    , "names" =  { idx <- order(names(x), decreasing=decreasing) }
  )
  ci <- ci[idx,]

  PlotDotCI( ci, xlim=xlim, ... )
    
}
