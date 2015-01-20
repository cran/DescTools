Second <-
function(x) {
#  strptime(x, "%S")  
  as.POSIXlt(x)$sec
}
