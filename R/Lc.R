Lc <-
function(x, n = rep(1, length(x)), na.rm = FALSE, plot = FALSE) {

  if(na.rm) x <- na.omit(x)
  if (any(is.na(x))) return(NA_real_)
  
  k <- length(x)
  o <- order(x)
  x <- x[o]
  n <- n[o]
  x <- n*x
  p <- cumsum(n)/sum(n)
  L <- cumsum(x)/sum(x)
  p <- c(0,p)
  L <- c(0,L)
  L2 <- L * mean(x)
  Lc <- list(p,L,L2)
  names(Lc) <- c("p", "L", "L.general")
  class(Lc) <- "Lc"
  
  if(plot) plot(Lc)
  Lc
}
