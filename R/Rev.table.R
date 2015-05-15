Rev.table <-
function(x, margin, ...) {
  
  if (!is.array(x)) 
    stop("'x' is not an array")
  
  newdim <- rep("", length(dim(x)))
  newdim[margin] <- paste(dim(x), ":1", sep="")[margin]
  z <- eval(parse(text=gettextf("x[%s]", paste(newdim, sep="", collapse=","))))
  class(z) <- oldClass(x)  
  return(z)
  
}
