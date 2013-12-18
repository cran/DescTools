Fibonacci <-
function(n, sequence = TRUE) {
  if (!is.numeric(n) || length(n) != 1 || floor(n) != ceiling(n) || n < 0)
    stop("Argument 'n' must be a single integer >= 0.")
  if (n <= 1) return(c(1))
  
  if (sequence) {
    if (n == 2) return(c(1, 2))
    fib <- numeric(n)
    fib[1:2] <- c(1, 2)
    for (k in 3:n) {
      fib[k] <- fib[k-1] + fib[k-2]
    }
  } else {
    if (n <= 1) {
      return(1)
    } else {
      fib = Fibonacci(n-1) + Fibonacci(n-2)
    }
  }
  return(fib)
}
