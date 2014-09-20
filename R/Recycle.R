Recycle <-
function(...){
  lgp <- list(...)
  maxdim <- max(unlist(lapply(lgp, length)))
  # recycle all params to maxdim
  res <- lapply(lgp, rep, length.out=maxdim)
  attr(res, "maxdim") <- maxdim
  
  return(res)
}
