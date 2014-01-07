Skew <-
function (x, na.rm = FALSE, method = 3, conf.level = NA, type = "bca", R = 1000) {

  i.skew <- function(x, na.rm = FALSE, method = 3) { 
    if (na.rm) x <- na.omit(x)
    
    n <- length(x)
    std <- (sum((x - mean(x))^2)/n)^0.5
    z3 <- 1/n * sum((x - mean(x))^3)
    # method 1: older textbooks
    r.skew <- z3/std^3 
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
