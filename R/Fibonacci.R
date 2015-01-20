Fibonacci <-
function(n) {
  
  if (!is.numeric(n) || !IsWhole(n) || n < 0)
    stop("Argument 'n' must be integer >= 0.")
  
  maxn <- max(n)
  if (maxn == 0) return(0)
  if (maxn == 1) return(c(0, 1)[n+1])  
  if (maxn == 2) return(c(0, 1, 1)[n+1])  
  z <- c(0, 1, 1, rep(NA, maxn-3))
  for (i in 4:(maxn+1)) {
    z[i] <- z[i-1] + z[i-2]
  }
  
  z[n+1]
  
}
