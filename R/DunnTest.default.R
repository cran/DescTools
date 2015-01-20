DunnTest.default <-
function (x, g, method = c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none"),
                              out.list = TRUE, ...) 
{
  if (is.list(x)) {
    if (length(x) < 2L) 
      stop("'x' must be a list with at least 2 elements")
    DNAME <- deparse(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    k <- length(x)
    l <- sapply(x, "length")
    if (any(l == 0)) 
      stop("all groups must contain data")
    g <- factor(rep(1:k, l))
    x <- unlist(x)
  }
  else {
    if (length(x) != length(g)) 
      stop("'x' and 'g' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    if (!all(is.finite(g))) 
      stop("all group levels must be finite")
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2) 
      stop("all observations are in the same group")
  }
  N <- length(x)
  if (N < 2) 
    stop("not enough observations")
  
  method <- match.arg(method)
  
  nms <- levels(g)
  
  n <- tapply(g, g, length)
  rnk <- rank(x)
  mrnk <- tapply(rnk, g, mean)
  
  tau <- table(rnk[AllDuplicated(rnk)])
  tiesadj <- sum(tau^3 - tau) / (12*(N-1))
  mrnkdiff <- outer(mrnk, mrnk, "-")
  
  z <- mrnkdiff / sqrt( ((N*(N+1)/12) - tiesadj) * outer(1/n, 1/n, "+"))
  pvals <- pnorm(abs(z), lower.tail=FALSE)
  
  keep <- lower.tri(pvals)
  pvals <- pvals[keep]
  m <- sum(keep)
  
  out <- list()
  
  pvals <- p.adjust(pvals, method=method) 
  method.str <- method
  
  if(out.list){
    dnames <- list(NULL, c("mean rank diff", "pval"))
    if (!is.null(nms)) 
      dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
    out[[1]] <- array(c(mrnkdiff[keep], pvals), c(length(mrnkdiff[keep]), 2L), dnames)
    
  } else {
    out[[1]] <- matrix(NA, nrow=length(nms), ncol=length(nms))
    out[[1]][lower.tri(out[[1]], diag = FALSE)] <- pvals
    dimnames(out[[1]]) <- list(nms, nms)
    out[[1]] <- out[[1]][-1, -ncol(out[[1]])]
    
  }
  
  class(out) <- c("DunnTest")
  attr(out, "method") <- method.str
  attr(out, "out.list") <- out.list
  
  return(out)
  
}
