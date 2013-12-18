lines.Lc <-
function(x, general=FALSE, lwd=2, ...) {

  if(!general)
    L <- x$L
  else
    L <- x$L.general
  lines(x$p, L, lwd=lwd, ...)
}
