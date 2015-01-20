`%overlaps%` <-
function(x, y) { 
  if(length(x) < 2) x <- rep(x, 2)
  if(length(y) < 2) y <- rep(y, 2)
  return(!(max(x) < min(y) | min(x) > max(y)) )
}
