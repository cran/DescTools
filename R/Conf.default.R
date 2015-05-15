Conf.default <-
function(x, ref, pos = NULL, na.rm = TRUE, ...) {
  if(na.rm) {
    idx <- complete.cases(data.frame(x, ref))
    x <- x[idx]
    ref <- ref[idx]
  }
  Conf.table(table(pred=x, obs=ref), pos = pos, ...)
}
