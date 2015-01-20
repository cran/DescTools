Sort.data.frame <-
function(x, ord = NULL, decreasing = FALSE, factorsAsCharacter = TRUE, 
                            na.last = TRUE, ...) {
  
  # why not using ord argument as in matrix and table instead of ord?
  
  if(is.null(ord)) { ord <- 1:ncol(x) }
  
  if(is.character(ord)) { 
    ord <- match(ord, c("row.names", names(x)))
  } else if(is.numeric(ord)) {
    ord <- as.integer(ord) + 1
  }  
  
  # recycle decreasing and by
  lgp <- list(decreasing = decreasing, ord = ord)
  # recycle all params to maxdim = max(unlist(lapply(lgp, length)))
  lgp <- lapply(lgp, rep, length.out = max(unlist(lapply(lgp, length))))
  # decreasing is not recycled in order, so we use rev to change the sorting direction
  # old: d.ord <- x[,lgp$ord, drop=FALSE]  # preserve data.frame with drop = FALSE
  d.ord <- data.frame(rn=rownames(x),x)[, lgp$ord, drop = FALSE] # preserve data.frame with drop = FALSE
  if(factorsAsCharacter){
    for( xn in which(sapply(d.ord, is.factor)) ){ d.ord[,xn] <- factor(d.ord[,xn], levels=sort(levels(d.ord[,xn]))) }
  }  
  
  d.ord[, which(sapply(d.ord, is.character))] <- lapply(d.ord[,which(sapply(d.ord, is.character)), drop=FALSE], factor)
  d.ord <- data.frame(lapply(d.ord, as.numeric))
  d.ord[lgp$decreasing] <- lapply(d.ord[lgp$decreasing], "-")
  
  x[ do.call("order", c(as.list(d.ord), na.last=na.last)), , drop = FALSE]
}
