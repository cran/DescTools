`%overlaps%` <-
function(xp, yp) { 
  if(length(xp) < 2) x <- rep(xp, 2)
  if(length(yp) < 2) y <- rep(yp, 2)
  return(!(max(xp) < min(yp) | min(xp) > max(yp)) )
}
