MutInf <-
function(x, y = NULL, base = 2, ...){
  # ### Ref.:  http://en.wikipedia.org/wiki/Cluster_labeling
  
  if(!is.null(y)) { x <- table(x, y, ...) }
  x <- as.matrix(x)
  
  return( 
    Entropy(apply(x, 1, sum), base=base) + 
      Entropy(apply(x, 2, sum), base=base) - Entropy(x, base=base)
  )  
  
}
