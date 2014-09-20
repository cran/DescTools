Trim <-
function(x, trim=0.1, na.rm=FALSE){
  
  if (na.rm) x <- na.omit(x)
  
  if (!is.numeric(trim) || length(trim) != 1L) 
    stop("'trim' must be numeric of length one")
  
  n <- length(x)
  
  if (trim > 0 && n) {
    if (is.complex(x)) 
      stop("trim is not defined for complex data")
    if (anyNA(x)) 
      return(NA_real_)
    if (trim >= 0.5) 
      return(stats::median(x, na.rm = FALSE))
    lo <- floor(n * trim) + 1
    hi <- n + 1 - lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  return(x)
}
