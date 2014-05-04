IsWeekend <-
function(x) {
  x <- as.POSIXlt(x)
  x$wday > 5 | x$wday < 1
}
