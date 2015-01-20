MeanSE <-
function(x, sd = NULL, na.rm = FALSE) {
  if(na.rm) x <- na.omit(x)
  if(is.null(sd)) s <- sd(x)
  s/sqrt(length(x))
}
