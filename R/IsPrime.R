IsPrime <-
function(x) {
  if (is.null(x) || length(x) == 0)
    stop("Argument 'x' must be a nonempty vector or matrix.")
  if (!is.numeric(x) || any(x < 0) || any(x != round(x)))
    stop("All entries of 'x' must be nonnegative integers.")
  
  n <- length(x)
  X <- x[1:n]
  L <- logical(n)
  p <- Primes(ceiling(sqrt(max(x))))
  for (i in 1:n) {
    L[i] <- all(X[i] %% p[p < X[i]] != 0)
  }
  L[X == 1 | X == 0] <- FALSE
  dim(L) <- dim(x)
  return(L)
}
