Conf.matrix <-
function(x, pos = NULL, ...) {
  Conf.table(as.table(x), pos=pos, ...)
}
