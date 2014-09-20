Desc.ordered <-
function(x, main = NULL, ...){  
  if( is.null(main)) main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  Desc.factor(x, main = main, ord = "level", ...)  
  invisible()
  
}
