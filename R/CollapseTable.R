CollapseTable <-
function (x, ...) {
  
  nargs <- length(args <- list(...))
  if (!nargs) 
    return(x)
  
  if (inherits(x, "ftable")) 
    x <- as.table(x)
  
  if (inherits(x, "table")) {
    tvars <- names(dimnames(x))
    x <- as.data.frame.table(x)
    freq <- x[, "Freq"]
  } else {
    stop("Argument must be a table or ftable object")
  }
  
  names <- names(args)
  
  for (i in 1:nargs) {
    vals <- args[[i]]
    nm <- names[[i]]
    if (any(nm == tvars)) 
      levels(x[[nm]]) <- vals
    else warning(nm, " is not among the x variables.")
  }
  
  xtabs(as.formula(paste("freq ~", paste(tvars, collapse = "+"))), 
        data = x)
  
}
