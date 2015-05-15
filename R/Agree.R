Agree <-
function(x, tolerance = 0, na.rm = FALSE) {
  
  x <- as.matrix(x)
  if(na.rm) x <- na.omit(x)
  
  if(anyNA(x)) return(NA)

  ns <- nrow(x)
  nr <- ncol(x)
  
  if (is.numeric(x)) {
    rangetab <- apply(x, 1, max) - apply(x, 1, min)
    coeff <-  sum(rangetab <= tolerance)/ns
    
  } else {
    rangetab <- as.numeric(sapply(apply(x, 1, table), length))
    coeff <- (sum(rangetab == 1)/ns)
    tolerance <- 0
  }
  
  rval <- coeff
  attr(rval, c("subjects")) <- ns
  attr(rval, c("raters")) <- nr
  
  return(rval)
  
}
