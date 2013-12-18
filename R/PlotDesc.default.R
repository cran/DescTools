PlotDesc.default <-
function(x, ...) {
  print( gettextf("Unhandled class %s of %s.\n", class(x), deparse(substitute(x)) ) )
}
