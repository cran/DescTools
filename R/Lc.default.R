Lc.default <-
function(x, n = rep(1, length(x)), na.rm = FALSE, ...) {

  g <- Gini(x, n, na.rm=na.rm)
  
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  
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
  Lc <- list(p, L, L2, g)
  names(Lc) <- c("p", "L", "L.general", "Gini")
  class(Lc) <- "Lc"
  
  # no plot anymore, we have plot(lc) and Desc(lc, plotit=TRUE)
  # if(plot) plot(Lc)
  Lc
}
