Skew <-
function (x, na.rm = FALSE, method = 3, conf.level = NA, type = "bca", R = 1000) {

  # C part for the expensive (x - mean(x))^2 etc. is a kind of 14 times faster
  #   > x <- rchisq(100000000, df=2)
  #   > system.time(Skew(x))
  #   user  system elapsed 
  #   6.32    0.30    6.62 
  #   > system.time(Skew2(x))
  #   user  system elapsed 
  #   0.47    0.00    0.47 
  
  
  i.skew <- function(x, na.rm = FALSE, method = 3) { 
    if (na.rm) x <- na.omit(x)
    
    n <- length(x)
    
    # method 1: older textbooks
    r.skew <- .Call("rskew", as.numeric(x), as.numeric(mean(x)), PACKAGE="DescTools")
    
    if (method == 2) {
      # method 2: SAS/SPSS
      r.skew <- r.skew * n^0.5 * (n - 1)^0.5/(n - 2)
    }
    else if (method == 3) {
      # method 3: MINITAB/BDMP
      r.skew <- r.skew * ((n - 1)/n)^(3/2)
    }
    return(r.skew)
  }
  
  if(is.na(conf.level)){
    res <- i.skew(x, na.rm=na.rm, method=method)    
    
  } else {
    
    # Problematic standard errors and confidence intervals for skewness and kurtosis.
    # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
    # adjusted bootstrap percentile (BCa) interval  
    boot.skew <- boot(x, function(x, d) i.skew(x[d], na.rm=na.rm, method=method), R=R)
    ci <- boot.ci(boot.skew, conf=conf.level, type=type)
    res <- c(skewness=boot.skew$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
  }
  
  return(res)
  
}
