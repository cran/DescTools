LogLin <-
function(x, a) {
  # log-linear hybrid transformation
  # introduced by Rocke and Durbin (2003)
  x[x<=a] <- x[x<=a] / a + log(a) - 1 
  x[x>a] <- log(x[x>a])

  return(x)  
}
