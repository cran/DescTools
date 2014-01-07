LCM <-
function(x) {
  
  
  .LCM <- function(n, m) {
    stopifnot(is.numeric(n), is.numeric(m))
    if (length(n) != 1 || floor(n) != ceiling(n) ||
          length(m) != 1 || floor(m) != ceiling(m))
      stop("Arguments 'n', 'm' must be integer scalars.")
    if (n == 0 && m == 0) return(0)
    
    return(n / GCD(c(n, m)) * m)
  }
  
  stopifnot(is.numeric(x))
  if (floor(x) != ceiling(x) || length(x) < 2)
    stop("Argument 'x' must be an integer vector of length >= 2.")
  
  x <- x[x != 0]
  n <- length(x)
  if (n == 0) {
    l <- 0
  } else if (n == 1) {
    l <- x
  } else if (n == 2) {
    l <- .LCM(x[1], x[2])
  } else {
    l <- .LCM(x[1], x[2])
    for (i in 3:n) {
      l <- .LCM(l, x[i])
    }
  }
  return(l)
}
