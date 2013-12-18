YuleQ <-
function(x, y = NULL, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  # allow only 2x2 tables
  stopifnot(prod(dim(x)) == 4 || length(x) == 4)

   a <- x[1,1]
   b <- x[1,2]
   c <- x[2,1]
   d <- x[2,2]
  return((a*d- b*c)/(a*d + b*c))  #Yule Q
  
}
