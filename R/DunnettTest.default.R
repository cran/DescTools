DunnettTest.default <-
function (x, g, control = NULL
                                  , conf.level = 0.95, ...) {
  
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
  } else {
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
  
  
  
  # just organisational stuff so far, got a fine x and g now
  
  if (is.null(control)) control <- levels(g)[1]
  
  ni <- tapply(x, g, length)
  
  means <- tapply(x, g, mean)
  meandiffs <- means[names(means) != control] - means[control]
  
  fittedn <- ni[names(ni) != control]
  controln <- ni[control]
  
  s <- sqrt( sum(tapply(x, g, function(x) sum((x - mean(x))^2) )) / 
               (N - k))
  
  Dj <- meandiffs / (s * sqrt((1/fittedn) + (1/controln)))
  Rij <- sqrt(fittedn/(fittedn + controln))
  
  R <- outer(Rij, Rij, "*")
  diag(R) <- 1
  
  set.seed(5)  # for getting consistent results every run
  qvt <- mvtnorm::qmvt((1 - (1 - conf.level)/2), df = N - k, sigma = R, tail = "lower.tail")$quantile
  
  lower <- meandiffs - s * sqrt((1/fittedn) + (1/controln)) * qvt
  upper <- meandiffs + s * sqrt((1/fittedn) + (1/controln)) * qvt
  
  pval <- c()
  for (i in 1:(k-1)){
    pval[i] <- 1 - mvtnorm::pmvt(-abs(Dj[i]), abs(Dj[i]), corr=R, delta=rep(0, k-1), df=N - k)[1]
  }
  
  out <- list()
  
  out[[1]] <- cbind(diff=meandiffs, lower, upper, pval)
  dimnames(out[[1]]) <- list(paste(names(meandiffs), control, sep="-"), c("diff", "lwr.ci", "upr.ci","pval"))
  names(out) <- control
  
  class(out) <- c("PostHocTest")
  attr(out, "orig.call") <- NA
  attr(out, "conf.level") <- conf.level
  attr(out, "ordered") <- FALSE
  attr(out, "method") <- ""
  attr(out, "method.str") <- gettextf("\n  Dunnett's test for comparing several treatments with a control : %s \n", attr(out, "method"))
  
  return(out)
  
}
