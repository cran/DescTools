Rev.table <-
function(x, direction = c("row","column","both"), ...) {
  
  direction <- match.arg(direction, choices=c("row","column","both"))
  
  if(direction != "row") x <- x[, ncol(x):1L]
  if(direction != "column") x <- x[nrow(x):1L,]
  return(x)  
}
