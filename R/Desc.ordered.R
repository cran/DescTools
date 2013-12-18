Desc.ordered <-
function(x, xname = NULL, ...){  
  if( is.null(xname)) xname <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  Desc.factor(x, xname = xname, ord = "level", ...)  
}
