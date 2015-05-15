Atkinson <-
function(x, n = rep(1, length(x)), parameter = 0.5, na.rm = FALSE) {

  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  
  if(is.null(parameter)) parameter <- 0.5
  if(parameter==1)
    A <- 1 - (exp(mean(log(x)))/mean(x))
  else
  {
    x <- (x/mean(x))^(1-parameter)
    A <- 1 - mean(x)^(1/(1-parameter))
  }
  A
}
