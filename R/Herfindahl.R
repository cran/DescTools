Herfindahl <-
function(x, n = rep(1, length(x)), parameter=1, na.rm = FALSE) {
  
  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  
  if(is.null(parameter))
    m <- 1
  else
    m <- parameter
  Herf <- x/sum(x)
  Herf <- Herf^(m+1)
  Herf <- sum(Herf)^(1/m)
  Herf
}
