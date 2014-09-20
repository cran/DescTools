YearDay <-
function(x) {
  return(as.integer(format(as.Date(x), "%j")))
}
