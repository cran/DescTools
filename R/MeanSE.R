MeanSE <-
function(x, na.rm = FALSE) {
  if(na.rm) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}
