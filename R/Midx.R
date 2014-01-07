Midx <-
function(x, first=NULL){
  if(!is.null(first)) x <- c(first, x)
  res <- cumsum(filter(x, rep(1/2,2)))  
  res <-  res[-length(res)]
  return(res)
}
