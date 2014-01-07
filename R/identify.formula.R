identify.formula <-
function(x, data = NULL, ...) {
  mf <- model.frame(x, data)
  x <- mf[,2]
  y <- mf[,1]
  identify(x, y, ...)
}
