Desc.list <-
function(x, xname = NULL, ...) {
  if(is.null(xname)) xname <- names(x)
  for(i in 1:length(x)){
    Desc(x[[i]], xname=xname[i], ...)
  }  
}
