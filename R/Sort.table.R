Sort.table <-
function (x, ord = NULL, decreasing = FALSE, na.last = TRUE, ...) {
  
  if (length(dim(x)) == 1 ){
    # do not specially handle 1-dimensional tables
    res <- sort(x=x, decreasing=decreasing)
    
  } else {
    if (is.null(ord)) {
      ord <- 1:ncol(x)
    }
    lgp <- list(decreasing = decreasing, ord = ord)
    lgp <- lapply(lgp, rep, length.out = max(unlist(lapply(lgp, length))))
    
    d.x <- data.frame(cbind( rownr=as.numeric(factor(row.names(x))), x, mar=apply(x, 1, sum)))
    d.ord <- d.x[, lgp$ord + 1, drop = FALSE]
    d.ord[lgp$decreasing] <- lapply(d.ord[lgp$decreasing], "-")
    
    res <- x[do.call("order", c(as.list(d.ord), na.last=na.last)), , drop=FALSE]
    class(res) <- "table"
  }
  
  return(res)
  
}
