Hmean <-
function(x, na.rm = FALSE) {
  if(any(x < 0)) return(NA)
  else
  return( 1 / mean(1/x, na.rm = na.rm) )
}
