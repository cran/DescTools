IsZero <-
function(x, tol = .Machine$double.eps^0.5, na.rm=FALSE) {
  # Define check if a numeric is 0 
  
  if (na.rm) 
    x <- na.omit(x)
  if(is.numeric(x))
    x < tol
  else
    FALSE
  
}
