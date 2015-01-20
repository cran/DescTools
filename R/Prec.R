Prec <-
function (x) {
    
  # Function to return the most precise
  # digit from a vector of real numbers
  # Keep dividing by powers of 10 (pos and neg from trunc(log(max(x)) down)
  # until the fractional portion is zero, then we have the highest precision
  # digit in terms of a integer power of 10.
  
  # Thanks to Thomas Lumley for help with machine precision
  
  # Note:  Turn this into a standalone function for "regularizing" a
  #        time-activity object with irregular time breaks.
  
  init <- trunc(log10(max(x))) + 1
  zero <- 0
  y <- 1
  while (any(y > zero)) {
    init <- init - 1
    x1 <- x*10^(-init)
    y <- x1 - trunc(x1)
    zero <- max(x1)*.Machine$double.eps
  }
  10^init
  
  # sapply(c(1.235, 125.3, 1245), prec)
  
}
