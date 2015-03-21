YearDay <-
function(x) {
  # return(as.integer(format(as.Date(x), "%j")))
  as.POSIXlt(x)$yday
}
