Gini <-
function(x, n = rep(1, length(x)), unbiased = TRUE, conf.level = NA, R = 1000, type = "bca", na.rm = FALSE) {

  x <- rep(x, n)    # same handling as Lc
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  
  i.gini <- function (x, unbiased = TRUE){
    n <- length(x)
    x <- sort(x)
    
    res <- 2 * sum(x * 1:n) / (n*sum(x)) - 1 - (1/n)
    if(unbiased) res <- n / (n - 1) * res 
    
# limit Gini to 0 here, if negative values appear, which is the case with
# Gini( c(10,10,10))
    return( pmax(0, res))

# other guy out there:
#     N <- if (unbiased) n * (n - 1) else n * n
#     dsum <- drop(crossprod(2 * 1:n - n - 1, x))
#     dsum / (mean(x) * N)
# is this slower, than above implementation??
  }
  
  if(is.na(conf.level)){
    res <- i.gini(x, unbiased = unbiased)    
    
  } else {
    # adjusted bootstrap percentile (BCa) interval  
    boot.gini <- boot(x, function(x, d) i.gini(x[d], unbiased = unbiased), R=R)
    ci <- boot.ci(boot.gini, conf=conf.level, type=type)
    res <- c(gini=boot.gini$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
  }
  
  return(res)
  
}
