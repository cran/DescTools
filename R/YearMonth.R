YearMonth <-
function(x){
  # returns the yearmonth representation of a date x
  x <- as.POSIXlt(x)
  return((x$year + 1900)*100 + x$mon + 1)
}
