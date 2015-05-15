NemenyiTest.default <-
function (x, g, 
                                 dist = c("tukey", "chisq"), out.list = TRUE, ...) {
  
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
  
  dist <- match.arg(dist, c("tukey", "chisq"))
  
  nms <- levels(g)
  
  n <- tapply(g, g, length)
  rnk <- rank(x)
  mrnk <- tapply(rnk, g, mean)
  
  tau <- table(rnk[AllDuplicated(rnk)])
  tiesadj <- min(1, 1 - sum(tau^3 - tau) / (N^3 - N))
  mrnkdiff <- outer(mrnk, mrnk, "-")
  
  if(dist == "chisq"){
    chi <- mrnkdiff^2 / ((N*(N+1)/12) * outer(1/n, 1/n, "+"))
    pvals <- pchisq(tiesadj * chi, df=k-1, lower.tail=FALSE)
  } else {
    z <- abs(mrnkdiff) / sqrt( (N*(N+1)/12) * outer(1/n, 1/n, "+"))
    pvals <- ptukey(z * sqrt(2), nmeans=k, df=Inf, lower.tail=FALSE)
  }
  
  
  keep <- lower.tri(pvals)
  pvals <- pvals[keep]
  m <- sum(keep)
  
  out <- list()
  
  # no p.adjustment in this test
  # pvals <- p.adjust(pvals, method=method) 
  method.str <- "none" #method
  
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
  attr(out, "main") <- gettextf("Nemenyi's test of multiple comparisons for independent samples (%s) ", dist)
  attr(out, "method") <- method.str
  attr(out, "out.list") <- out.list
  
  return(out)
  
}
