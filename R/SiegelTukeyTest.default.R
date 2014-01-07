SiegelTukeyTest.default <-
function(x, y, adjust.median = FALSE, 
    alternative = c("two.sided","less","greater"), mu = 0,  
    exact = NULL, correct = TRUE, conf.int = FALSE, conf.level = 0.95, ...) {
    ###### published on:
    #   http://www.r-statistics.com/2010/02/siegel-tukey-a-non-parametric-test-for-equality-in-variability-r-code/
    #   Main author of the function:  Daniel Malter

    # Doku: http://www.crcnetbase.com/doi/abs/10.1201/9781420036268.ch14
 
  
  if (!missing(mu) && ((length(mu) > 1L) || !is.finite(mu))) 
      stop("'mu' must be a single number")

  if (conf.int) {
      if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
          (conf.level > 0) && (conf.level < 1))) 
          stop("'conf.level' must be a single number between 0 and 1")
  }
  
  if (!is.numeric(x)) 
      stop("'x' must be numeric")
      
  if (!is.null(y)) {
      if (!is.numeric(y)) 
          stop("'y' must be numeric")
      DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
      x <- x[is.finite(x)]
      y <- y[is.finite(y)]
  }
  else {
      DNAME <- deparse(substitute(x))
      x <- x[is.finite(x)]
  }
 
  # adjusting median
  if (adjust.median) {
      x <- x - median(x)
      y <- y - median(y)
  }

  # the larger group comes first
  if( length(x) > length(y) ){
    xx <- c(x, y)
    id <- c(rep(0, length(x)), rep(1, length(y)))
  } else {
    xx <- c(y,x)
    id <- c(rep(0, length(y)), rep(1, length(x)))
  }  

  strank <- SiegelTukeyRank(xx, g = id)
  ranks0 <- strank$unique.ranks[strank$sort.id == 0]
  ranks1 <- strank$unique.ranks[strank$sort.id == 1]
  
  RVAL <- wilcox.test(ranks0, ranks1, alternative = alternative, 
      mu = mu, paired = FALSE, exact = exact, correct = correct, 
      conf.int = conf.int, conf.level = conf.level)
      
  RVAL$statistic <- sum(ranks1)
  names(RVAL$statistic)  <- "ST"
  RVAL$data.name <- DNAME
  RVAL <- c(RVAL, list(stranks = strank, MeanRanks = c(mean(ranks0), mean(ranks1))))
  RVAL$method <- "Siegel-Tukey-test for equal variability"
  RVAL$null.value <- 1
  names(RVAL$null.value) <- "ratio of scales"
  class(RVAL) <- "htest"
  return(RVAL)

  if(suppressWarnings(wilcox.test(x,y)$p.value) < 0.05) warning("SiegelTukeyTest: wilcox.test(x, y) is significant! Consider setting adjust.median = TRUE." )
  
}
