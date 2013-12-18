Mode <-
function(x, na.rm=FALSE) {
  if(!is.atomic(x) | is.matrix(x)) stop("Mode supports only atomic vectors. Use sapply(*, Mode) instead.")
  if(na.rm) x <- na.omit(x)
  tab <- table(x)
  res <- names( which(tab==max(tab)) )    # handle ties...
  if( !inherits(x,"factor")) class(res) <- class(x)
  return(as.vector(res))
}
