Flags <-
function(x){
  res <- x[, WhichFlags(x)]
  class(res) <- "flags"
  return(res)
}
