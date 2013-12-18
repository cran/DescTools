AddLoess.default <-
function (x, y, ...) {
  AddLoess(y ~ x, data = data.frame(x,y), ...)
}
