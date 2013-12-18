JonckheereTerpstraTest.default <-
function (x, g, alternative = c("two.sided", "increasing", "decreasing"), nperm=NULL, ...) {

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
    n <- length(x)
    if (n < 2) 
        stop("not enough observations")
    
    # start calculating    
    
  jtpdf <- function(gsize) {
    ng <- length(gsize)
    cgsize <- rev(cumsum(rev(gsize)))
    mxsum <- sum(gsize[-ng]*cgsize[-1]) + 1
    zz <- .Fortran("jtpdf",
                   as.integer(mxsum),
                   pdf=double(mxsum),
                   as.integer(ng),
                   as.integer(cgsize),
                   double(mxsum),
                   double(mxsum))
    zz$pdf
  }

  jtperm.p <- function(x, ng, gsize, cgsize, alternative, nperm) {
    # this function computes the pdf using the convolution by Mark van de Wiel
    
    n <- length(x)
    pjtrsum <- rep(0, nperm)
    for (np in 1:nperm){
      jtrsum <- 0
      for(i in 1:(ng-1)) {
        na <- gsize[i]
        nb <- n-cgsize[i+1]
  # this jtrsum will be small if data are increasing and large if decreasing
        jtrsum <- jtrsum + sum(rank(x[(cgsize[i]+1):n])[1:na]) - na*(na+1)/2
      }
      pjtrsum[np] <- jtrsum
      # permute the data; this way the first value is the original statistic
      x <- sample(x)
    }
    # one-sided p-values
    # number of permuted values at least as small as original
    iPVAL <- sum(pjtrsum <= pjtrsum[1])/nperm
    # number of permuted values at least as large as original
    dPVAL <- sum(pjtrsum >= pjtrsum[1])/nperm
    # return p-value for the alternative of interest
    PVAL <- switch(alternative,
                   "two.sided" = 2*min(iPVAL, dPVAL, 1),
                   "increasing" = iPVAL,
                   "decreasing" = dPVAL)
    PVAL
  }

  if(!is.numeric(x)) stop("data values should be numeric")
  if(!is.numeric(g) & !is.ordered(g)) stop("group should be numeric or ordered factor")
  alternative <- match.arg(alternative)
  METHOD <- "Jonckheere-Terpstra test"
  PERM <- !missing(nperm)
  n <- length(x)
  if(length(g) != n) stop("lengths of data values and group don't match")
  TIES <- length(unique(x)) != n
  gsize <- table(g)
  ng <- length(gsize)
  cgsize <- c(0,cumsum(gsize))
  x <- x[order(g)]
  jtrsum <- jtmean <- jtvar <- 0
  for(i in 1:(ng-1)) {
    na <- gsize[i]
    nb <- n-cgsize[i+1]
    jtrsum <- jtrsum + sum(rank(x[(cgsize[i]+1):n])[1:na]) - na*(na+1)/2
    jtmean <- jtmean + na*nb/2
    jtvar <- jtvar + na*nb*(na+nb+1)/12
  }
# this jtrsum will be small if data are increasing and large if decreasing
# to reverse this use 2*jtmean - jtrsum  
  jtrsum <- 2*jtmean - jtrsum
  STATISTIC <- jtrsum
  names(STATISTIC) <- "JT"
  if (PERM) {
    PVAL <- jtperm.p(x, ng, gsize, cgsize, alternative, nperm) 
  } else {
    if (n > 100 | TIES) {
      warning("Sample size > 100 or data with ties \n p-value based on normal approximation. Specify nperm for permutation p-value")
      zstat <- (STATISTIC-jtmean)/sqrt(jtvar)
      PVAL <- pnorm(zstat)
      PVAL <- switch(alternative,
                     "two.sided" = 2*min(PVAL, 1-PVAL, 1),
                     "increasing" = 1-PVAL,
                     "decreasing" = PVAL)
    } else {
      dPVAL <- sum(jtpdf(gsize)[1:(jtrsum+1)])
      iPVAL <- 1-sum(jtpdf(gsize)[1:(jtrsum)])
      PVAL <- switch(alternative,
                     "two.sided" = 2*min(iPVAL, dPVAL, 1),
                     "increasing" = iPVAL,
                     "decreasing" = dPVAL)
    }
  }
  
  RVAL <- list(statistic = STATISTIC,
               p.value = as.numeric(PVAL),
               alternative = alternative,
               method = METHOD,
               data.name = DNAME)
  class(RVAL) <- "htest"
  RVAL

}
