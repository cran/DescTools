AddLm.default <-
function(x, y, ...){
  AddLm(y ~ x, data = data.frame(x, y), ...)
}
