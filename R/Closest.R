Closest <-
function(x, a, which = FALSE, na.rm = FALSE){
  
#   # example: Closest(a=67.5, x=d.pizza$temperature)
# 
  if(na.rm) x <- na.omit(x)

  mdist <- min(abs(x-a))
  if(is.na(mdist)) 
    res <- NA
  
  else {
    idx <- DescTools::IsZero(abs(x-a) - mdist)    # beware of floating-point-gods
    if(which == TRUE )
      res <- which(idx)
    else
      res <- x[idx] 
  }
  
# Frank's Hmisc solution is faster
# but does not handle ties satisfactorily

#   res <- .Fortran("wclosest", as.double(a), as.double(x), length(a), 
#            length(x), j = integer(length(a)), PACKAGE = "DescTools")$j
#   if(!which) res <- x[res]
  return(res)
  
}
