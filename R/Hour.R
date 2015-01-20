Hour <-
function(x) {
  # strptime(x, "%H") 
  as.POSIXlt(x)$hour
}
