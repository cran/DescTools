Desc.ordered <-
function(x, main = NULL, ...){  
  if(is.null(main)) main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  if(is.null(list(...)$ord)){      
    Desc.factor(x, main = main, ord = "level", ...)  
  } else {
    Desc.factor(x, main = main, ...)  
  }  
  invisible()
  
}
