LehmacherTest <-
function(x, y = NULL) {
  
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
  
  rsum <- rowSums(x)
  csum <- colSums(x)
  
  STATISTIC <- (rsum-csum)^2 / (rsum + csum - 2*diag(x))
  PARAMETER <- 1
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
  METHOD <- "Lehmacher-Test on Marginal Homogeneity"
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = STATISTIC, parameter = PARAMETER, 
                 p.value = PVAL, p.value.corr = p.adjust(PVAL, "hochberg"), 
                 method = METHOD, data.name = DNAME), 
            class = "mtest")
  
}
