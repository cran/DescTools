ToLong <-
function(x, varnames=NULL){
  lst <- as.list(x)
  res <- data.frame(rep(names(lst), lapply(lst, length)), unlist(lst))
  rownames(res) <- NULL
  if(is.null(varnames)) varnames <- c("grp","x")
  colnames(res) <- varnames
  return(res)                    
}
