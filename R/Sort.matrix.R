Sort.matrix <-
function (x, ord = NULL, decreasing = FALSE, na.last = TRUE, ...) {
  
  if (length(dim(x)) == 1 ){
    # do not specially handle 1-dimensional matrices
    res <- sort(x=x, decreasing=decreasing)
    
  } else {
    if (is.null(ord)) {
      # default order by sequence of columns
      ord <- 1:ncol(x)
    }
  
    # replace keyword by code
    ord[ord=="row_names"] <- 0
    # we have to coerce, as ord will be character if row_names is used
    ord <- as.numeric(ord)
    
    lgp <- list(decreasing = decreasing, ord = ord)
    lgp <- lapply(lgp, rep, length.out = max(unlist(lapply(lgp, length))))
    
    if( is.null(row.names(x))) {
      d.x <- data.frame(cbind(rownr=1:nrow(x)), x)
    } else {
      d.x <- data.frame(cbind( rownr=as.numeric(factor(row.names(x))), x))
    }
    d.ord <- d.x[, lgp$ord + 1, drop = FALSE]
    d.ord[lgp$decreasing] <- lapply(d.ord[lgp$decreasing], "-")
    
    res <- x[do.call("order", c(as.list(d.ord), na.last=na.last)), , drop=FALSE]
    # old version cannot be used for [n,1]-matrices, we switch to reset dim
    # class(res) <- "matrix"
    # 19.9.2013: dim kills rownames, so stick to drop = FALSE
    # dim(res) <- dim(x)
  }
  
  return(res)
  
}
