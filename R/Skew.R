Skew <-
function (x, na.rm = FALSE, method = 3, conf.level = NA, ci.type = "bca", R=1000, ...) {

  # C part for the expensive (x - mean(x))^2 etc. is a kind of 14 times faster
  #   > x <- rchisq(100000000, df=2)
  #   > system.time(Skew(x))
  #   user  system elapsed 
  #   6.32    0.30    6.62 
  #   > system.time(Skew2(x))
  #   user  system elapsed 
  #   0.47    0.00    0.47 
  
  
  i.skew <- function(x, method = 3) { 
    
    n <- length(x)
    
    # method 1: older textbooks
    r.skew <- .Call("rskew", as.numeric(x), as.numeric(mean(x)), PACKAGE="DescTools")
    se <- sqrt((6*(n-2))/((n+1)*(n+3)))
    
    if (method == 2) {
      # method 2: SAS/SPSS
      r.skew <- r.skew * n^0.5 * (n - 1)^0.5/(n - 2)
      se <- se * sqrt(n*(n-1))/(n-2)
    }
    else if (method == 3) {
      # method 3: MINITAB/BDMP
      r.skew <- r.skew * ((n - 1)/n)^(3/2)
      se <- se * ((n - 1)/n)^(3/2)
    }
    return(c(r.skew, se^2))
  }
  
  if (na.rm) x <- na.omit(x)

  if(is.na(conf.level)){
    res <- i.skew(x, method=method)[1]
    
  } else {
    
    if(ci.type == "classic") {
      res <- i.skew(x, method=method)
      res <- c(skewness=res[1], lwr.ci=pnorm(1-(1-conf.level)/2) * sqrt(res[2]), upr.ci=pnorm(1-(1-conf.level)/2) * sqrt(res[2]))
      
    } else {
      # Problematic standard errors and confidence intervals for skewness and kurtosis.
      # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
      # adjusted bootstrap percentile (BCa) interval
      boot.skew <- boot(x, function(x, d) i.skew(x[d], method=method), R=R, ...)
      ci <- boot.ci(boot.skew, conf=conf.level, type=ci.type)
      if(ci.type =="norm") {
        lwr.ci <- ci[[4]][2]
        upr.ci <- ci[[4]][3]
      } else {
        lwr.ci <- ci[[4]][4]
        upr.ci <- ci[[4]][5]
      }  
    }  

    res <- c(skewness=boot.skew$t0[1], lwr.ci=lwr.ci, upr.ci=upr.ci)
    # res <- ci
  }
  
  return(res)
  
}
