Midx <-
function(x, incl.zero = FALSE, cumulate = FALSE){
  if(incl.zero) x <- c(0, x)
  res <- filter(x, rep(1/2,2))
  res <-  res[-length(res)]
  if(cumulate) res <- cumsum(res)
  return(res)
}
