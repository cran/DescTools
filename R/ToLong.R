ToLong <-
function (x, varnames = NULL) {
  if(!is.list(x)) {
    lst <- as.list(x)
  } else {
    lst <- x
  }
  grpnames <- names(lst)
  if(is.null(grpnames)) grpnames <- paste("X", 1:length(lst), sep="")
  res <- data.frame(rep(grpnames, lapply(lst, length)), unlist(lst))
  rownames(res) <- NULL
  if (is.null(varnames)) 
    varnames <- c("grp", "x")
  
  colnames(res) <- varnames
  rownames(res) <- do.call(paste, c(expand.grid(grpnames, rownames(x)), sep="."))
  
  return(res)
}
