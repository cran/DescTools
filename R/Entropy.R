Entropy <-
function(x, y = NULL, base = 2, ...) {

  # x is either a table or a vector if y is defined
  
  if(!is.null(y)) { x <- table(x, y, ...) }
  x <- as.matrix(x)

  ptab <- x / sum(x)
  H <- - sum( ifelse(ptab > 0, ptab * log(ptab, base=base), 0) )
  return(H)

}
