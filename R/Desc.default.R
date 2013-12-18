Desc.default <-
function (x, ...) {
  cat(gettextf("\nSorry, don't know how to handle class(es) %s (%s)!\n\n", 
               paste(class(x), collapse=", "), deparse(substitute(x))))
}
