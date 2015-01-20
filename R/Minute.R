Minute <-
function(x) {
#  strptime(x, "%M")  
  as.POSIXlt(x)$min
}
