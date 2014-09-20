ToWide <-
function(x, g, varnames=NULL){
  g <- factor(g)
  res <- do.call("cbind", split(x, g))
  if(is.null(varnames)) varnames <- levels(g)
  colnames(res) <- varnames
  return(res)
}
