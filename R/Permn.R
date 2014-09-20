Permn <-
function(x, sort = FALSE) {
  
  # by F. Leisch
  
  n <- length(x)
  
  if (n == 1) 
    return(matrix(1))
  else if (n < 2) 
    stop("n must be a positive integer")
  z <- matrix(1)
  for (i in 2:n) {
    y <- cbind(z, i)
    a <- c(1:i, 1:(i - 1))
    z <- matrix(0, ncol = ncol(y), nrow = i * nrow(y))
    z[1:nrow(y), ] <- y
    for (j in 2:i - 1) {
      z[j * nrow(y) + 1:nrow(y), ] <- y[, a[1:i + j]]
    }
  }
  dimnames(z) <- NULL
  
  m <- apply(z, 2, function(i) x[i])
  
  if(any(duplicated(x))) 
    m <- unique(m)
  
  if(sort) m <- Sort(m)
  return(m)
  
}
