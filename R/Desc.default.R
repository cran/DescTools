Desc.default <-
function (x, ...) {
  
  if(!is.null(class(x))) {
    cat(gettextf("\nSorry, don't know how to handle class(es) %s (%s)!\n\n", 
                 paste(class(x), collapse=", "), deparse(substitute(x))))
  } else {
    cat(gettextf("\nObject %s does not exist!\n\n", deparse(substitute(x))))
  }
}
