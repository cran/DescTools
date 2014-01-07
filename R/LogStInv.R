LogStInv <-
function (x, threshold = NULL) {
  
  if(is.null(threshold)) threshold <- attr(x, "threshold")
  
  res <- rep(NA, length(x))
  idx <- (x < log10(threshold))
  idx.na <- is.na(idx)
  res[idx & !idx.na] <- threshold - threshold * log(10) *( log10(threshold) - x[idx & !idx.na])
  res[!idx & !idx.na] <- 10^(x[!idx & !idx.na])
  
  return(res)
  
}
