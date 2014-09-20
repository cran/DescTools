StuartMaxwellTest <-
function (x, y = NULL) {
  
  # stuart.maxwell.mh computes the marginal homogeneity test for
  # a CxC matrix of assignments of objects to C categories or an
  # nx2 or 2xn matrix of category scores for n data objects by two
  # raters. The statistic is distributed as Chi-square with C-1
  # degrees of freedom.
  
  # The core code is form Jim Lemon, package concord
  # the intro is taken from mcnemar.test (core)
  
  if (is.matrix(x)) {
    r <- nrow(x)
    if ((r < 2) || (ncol(x) != r)) 
      stop("'x' must be square with at least two rows and columns")
    if (any(x < 0) || anyNA(x)) 
      stop("all entries of 'x' must be nonnegative and finite")
    DNAME <- deparse(substitute(x))
  }
  else {
    if (is.null(y)) 
      stop("if 'x' is not a matrix, 'y' must be given")
    if (length(x) != length(y)) 
      stop("'x' and 'y' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    r <- nlevels(x)
    if ((r < 2) || (nlevels(y) != r)) 
      stop("'x' and 'y' must have the same number of levels (minimum 2)")
    x <- table(x, y)
  }
  
  # get the marginals
  rowsums <- rowSums(x)
  colsums <- colSums(x)
  equalsums <- rowsums == colsums
  
  if(any(equalsums)) {
    # dump any categories with perfect agreement
    x <- x[!equalsums, !equalsums]
    # bail out if too many categories have disappeared
    if(dim(x)[1] < 2) stop("Too many equal marginals, cannot compute")
    # get new marginals
    rowsums <- rowSums(x)
    colsums <- colSums(x)
  }
  
  # use K-1 marginals
  Kminus1 <- length(rowsums) - 1
  smd <- (rowsums-colsums)[1:Kminus1]
  smS <- matrix(0, nrow=Kminus1, ncol=Kminus1)
  for(i in 1:Kminus1) {
    for(j in 1:Kminus1) {
      if(i == j) smS[i,j] <- rowsums[i] + colsums[j] - 2 * x[i,j]
      else smS[i,j] <- -(x[i,j] + x[j,i])
    }
  }
  
  STATISTIC <- t(smd) %*% solve(smS) %*% smd
  
  PARAMETER <- r - 1
  METHOD <- "Stuart-Maxwell test"
  
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  names(STATISTIC) <- "chi-squared"
  names(PARAMETER) <- "df"
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
               p.value = PVAL, method = METHOD, data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
  
}
