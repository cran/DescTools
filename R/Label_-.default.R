`Label<-.default` <-
function(x, ..., value) {
  if(is.list(value))  stop("cannot assign a list to be a object label")
  if((length(value) != 1L) & !is.null(value)) stop("value must be character vector of length 1")
 
  attr(x, "label") <- value
  return(x)
}
