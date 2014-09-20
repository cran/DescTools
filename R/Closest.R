Closest <-
function(x, a, na.rm = FALSE){

  # example: Closest(a=67.5, x=d.pizza$temperature)

  if(na.rm) x <- na.omit(x)

  mdist <- min(abs(x-a), na.rm=na.rm)
  if(is.na(mdist)) res <- NA
  else res <- x[which(abs(x-a) == mdist)] 
  
  return(res)
  
}
