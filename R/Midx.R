Midx <-
function(x){
  res <- filter(x, rep(1/2,2))
  res <-  res[-length(res)]
  return(res)
}
