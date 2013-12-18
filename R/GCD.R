GCD <-
function(x) {
  
  .GCD <- function(n, m) {
    stopifnot(is.numeric(n), is.numeric(m))
    if (length(n) != 1 || floor(n) != ceiling(n) ||
          length(m) != 1 || floor(m) != ceiling(m))
      stop("Arguments 'n', 'm' must be integer scalars.")
    if (n == 0 && m == 0) return(0)
    
    n <- abs(n); m <- abs(m)
    if (m > n) {
      t <- n; n <- m; m <- t
    }
    while (m > 0) {
      t <- n
      n <- m
      m <- t %% m
    }
    return(n)
  }
  
  
  stopifnot(is.numeric(x))
  if (floor(x) != ceiling(x) || length(x) < 2)
    stop("Argument 'x' must be an integer vector of length >= 2.")
  
  x <- x[x != 0]
  n <- length(x)
  if (n == 0) {
    g <- 0
  } else if (n == 1) {
    g <- x
  } else if (n == 2) {
    g <- .GCD(x[1], x[2])
  } else {
    g <- .GCD(x[1], x[2])
    for (i in 3:n) {
      g <- .GCD(g, x[i])
      if (g == 1) break
    }
  }
  return(g)
}
