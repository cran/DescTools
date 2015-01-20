Flags <-
function(x){
  res <- x[, sapply(x, IsDichotomous)]
  class(res) <- "flags"
  return(res)
}
