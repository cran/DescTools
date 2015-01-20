Agree <-
function(ratings, tolerance = 0, na.rm = FALSE) {
  
  ratings <- as.matrix(ratings)
  if(na.rm) ratings <- na.omit(ratings)

  ns <- nrow(ratings)
  nr <- ncol(ratings)
  
  if (is.numeric(ratings)) {
    rangetab <- apply(ratings, 1, max) - apply(ratings, 1, min)
    coeff <-  sum(rangetab <= tolerance)/ns
    
  } else {
    rangetab <- as.numeric(sapply(apply(ratings, 1, table), length))
    coeff <- (sum(rangetab == 1)/ns)
    tolerance <- 0
  }
  
  rval <- coeff
  attr(rval, c("subjects")) <- ns
  attr(rval, c("raters")) <- nr
  
  return(rval)
  
}
