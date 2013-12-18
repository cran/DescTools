Clockwise <-
function(x, start=0){
  # Calculates begin and end angles from a list of given angles
  angles <- c(0, cumsum(x), 2*pi)
  revang <- 2*pi - angles + start
  return(data.frame( from=revang[-1], to=revang[-length(revang)]))
}
