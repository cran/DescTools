`Label<-.data.frame` <-
function(x, self=TRUE, ..., value) {
  if(!is.data.frame(x))  stop("x must be a data.frame")

  if(self){
    attr(x, "label") <- value
  } else {
    for (i in seq(along.with=x)) {
      Label(x[[i]]) <- value[[i]]
    }
  }
  return(x)
}
