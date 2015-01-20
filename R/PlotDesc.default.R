PlotDesc.default <-
function(x, ...) {
  warning( gettextf("Unhandled class %s of %s.\n", class(x), deparse(substitute(x)) ) )
}
