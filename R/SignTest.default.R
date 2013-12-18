SignTest.default <-
function(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
    mu = 0, conf.level = 0.95, ...) {
    
  MedianCI_Binom <- function( x, conf.level = 0.95, 
      alternative = c("two.sided", "less", "greater"), na.rm = FALSE ){
    # http://www.stat.umn.edu/geyer/old03/5102/notes/rank.pdf
    # http://de.scribd.com/doc/75941305/Confidence-Interval-for-Median-Based-on-Sign-Test
    if(na.rm) x <- na.omit(x)
    n <- length(x)
    switch( match.arg(alternative)
      , "two.sided" = { 
          k <- qbinom(p = (1 - conf.level) / 2, size=n, prob=0.5, lower.tail=TRUE)
          ci <- sort(x)[c(k, n - k + 1)]
          attr(ci, "conf.level") <- 1 - 2 * pbinom(k-1, size=n, prob=0.5) 
        }
      , "greater" = { 
          k <- qbinom(p = (1 - conf.level), size=n, prob=0.5, lower.tail=TRUE)
          ci <- c(sort(x)[k], Inf)
          attr(ci, "conf.level") <- 1 - pbinom(k-1, size=n, prob=0.5) 
        }
      , "less" = { 
          k <- qbinom(p = conf.level, size=n, prob=0.5, lower.tail=TRUE)
          ci <- c(-Inf, sort(x)[k])
          attr(ci, "conf.level") <- pbinom(k, size=n, prob=0.5) 
        }
    )
    return(ci)
  }
    
  alternative <- match.arg(alternative)
    
  if (!missing(mu) && ((length(mu) > 1L) || !is.finite(mu))) 
      stop("'mu' must be a single number")

  if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
      (conf.level > 0) && (conf.level < 1))) 
      stop("'conf.level' must be a single number between 0 and 1")
  
  if (!is.numeric(x)) 
      stop("'x' must be numeric")
      
  if (!is.null(y)) {
      if (!is.numeric(y)) 
          stop("'y' must be numeric")
      if (length(x) != length(y)) 
          stop("'x' and 'y' must have the same length")

      DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
      OK <- complete.cases(x, y)
      x <- x[OK] 
      y <- y[OK]
      METHOD <- "Dependent-samples Sign-Test"
      x <- (x - y)
  
  } else {
      DNAME <- deparse(substitute(x))
      x <- x[is.finite(x)]
      METHOD <- "One-sample Sign-Test"
  }

  d <- (x - mu)

  # Naive version:
  n.valid <- sum(d>0)+sum(d<0)
  if(n.valid>0) {
    RVAL <- binom.test( x = sum(d>0), n=n.valid, p=0.5, alternative = alternative, conf.level = conf.level ) 
  } else {
    RVAL=binom.test(x = 1, n = 1)
  }
  # RVAL <- c(RVAL, list(STRanks = STRank, MeanRanks = c(mean(ranks0), mean(ranks1))))
  
  RVAL$method <- METHOD
  RVAL$data.name <- DNAME
  names(mu) <- if (!is.null(y)) "median difference" else "median"

  names(RVAL$statistic) <- "S"
  RVAL$estimate <- median(d + mu, na.rm=TRUE)
  names(RVAL$parameter) <- "number of differences"
  mci <- MedianCI_Binom(d + mu, conf.level=conf.level, alternative=alternative, na.rm=TRUE) 
  RVAL$conf.int <- mci
  attr(RVAL$conf.int, "conf.level") = round(attr(mci,"conf.level"),3)

  names(RVAL$estimate) <- "median of the differences"
  RVAL$null.value <- mu
  class(RVAL) <- "htest"
  return(RVAL)

}
