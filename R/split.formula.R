split.formula <-
function(x, f, drop = FALSE, data = NULL, ...) {
  mf <- model.frame(x, data)
  f <- mf[,2]
  x <- mf[,1]
  split(x, f, drop=drop, ...)
}
