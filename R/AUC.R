AUC <-
function(x, y, method=c("trapezoid", "step", "spline"), na.rm = FALSE) {
  
  # calculates Area unter the curve
  # example:
  #   AUC( x=c(1,2,3,5), y=c(0,1,1,2))
  #   AUC( x=c(2,3,4,5), y=c(0,1,1,2))
  
  if(na.rm) { 
    idx <- na.omit(cbind(x,y))
    x <- x[idx]
    y <- y[idx]
  }

  if (length(x) != length(y))
    stop("length x must equal length y")
  
  idx <- order(x)
  x <- x[idx]
  y <- y[idx]
  
  switch( match.arg( arg=method, choices=c("trapezoid","step","spline") )
    , "trapezoid" = { a <- sum((apply( cbind(y[-length(y)], y[-1]), 1, mean))*(x[-1] - x[-length(x)])) }
    , "step" = { a <- sum( y[-length(y)] * (x[-1] - x[-length(x)])) }
    , "spline" = { a <- integrate(splinefun(x, y, method="natural"), lower=min(x), upper=max(x))$value }
  )
  return(a)
}
