Rosenbluth <-
function(x, n = rep(1, length(x)), na.rm = FALSE) {
  
  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x))) return(NA_real_)
  
  n <- length(x)
  x <- sort(x)
  HT <- (n:1)*x
  HT <- 2*sum(HT/sum(x))
  HT <- 1/(HT-1)
  HT
}
