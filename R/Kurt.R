Kurt <-
function (x, na.rm = FALSE, method = 3, conf.level = NA, ci.type = "bca", R=1000, ...) {
  
  i.kurt <- function(x, na.rm = FALSE, method = 3) {
    if (na.rm) x <- na.omit(x)
    
    n <- length(x)
    # method 1: older textbooks
    r.kurt <- .Call("rkurt", as.numeric(x), as.numeric(mean(x)), PACKAGE="DescTools")
    se <- sqrt((24*n*(n-2)*(n-3))/((n+1)^2*(n+3)*(n+5)))
    
    if (method == 2) {
      # method 2: SAS/SPSS
      r.kurt <- ((r.kurt + 3) * (n + 1)/(n - 1) - 3) * (n - 1)^2/(n - 2)/(n - 3)
      se <- se * (((n-1)*(n+1))/((n-2)*(n-3)))
    }
    else if (method == 3) {
      # method 3: MINITAB/BDMP
      r.kurt <- (r.kurt + 3) * (1 - 1/n)^2 - 3
      se <- se * ((n-1)/n)^2
    }
    return(c(r.kurt, se^2))
  }
  
  if(is.na(conf.level)){
    res <- i.kurt(x, na.rm=na.rm, method=method)[1]
    
  } else {
    if(ci.type == "classic") {
      res <- i.kurt(x, method=method)
      res <- c(kurtosis=res[1], lwr.ci=qnorm(1-(1-conf.level)/2) * sqrt(res[2]), upr.ci=qnorm(1-(1-conf.level)/2) * sqrt(res[2]))
      
    } else {
      
      # Problematic standard errors and confidence intervals for skewness and kurtosis.
      # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
      # adjusted bootstrap percentile (BCa) interval  
      boot.kurt <- boot(x, function(x, d) i.kurt(x[d], na.rm=na.rm, method=method), R=R, ...)
      ci <- boot.ci(boot.kurt, conf=conf.level, type=ci.type)
      
      if(ci.type =="norm") {
        lwr.ci <- ci[[4]][2]
        upr.ci <- ci[[4]][3]
      } else {
        lwr.ci <- ci[[4]][4]
        upr.ci <- ci[[4]][5]
      }  
      
      res <- c(kurt=boot.kurt$t0[1], lwr.ci=lwr.ci, upr.ci=upr.ci)
    }
  }
  
  return(res)
  
}
