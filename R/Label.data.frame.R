Label.data.frame <-
function(x, ...) {
  labels <- mapply(FUN=Label, x=x)
  return(labels[unlist(lapply(labels, function(x) !is.null(x) ))])
}
