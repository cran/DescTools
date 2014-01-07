Kurt <-
function (x, na.rm = FALSE, method = 3, conf.level = NA, type = "bca", R = 1000) {

  i.kurt <- function(x, na.rm = FALSE, method = 3) {
    if (na.rm) x <- na.omit(x)

    n <- length(x)
  # method 1: older textbooks
    std <- (sum((x-mean(x))^2)/n)^0.5
    z4 <- 1/n*sum((x-mean(x))^4)
    r.kurt <- z4/std^4 - 3
    if (method == 2) {
  # method 2: SAS/SPSS
        r.kurt <- ((r.kurt + 3) * (n + 1)/(n - 1) - 3) * (n - 
        1)^2/(n - 2)/(n - 3)
    }
    else if (method == 3) {
  # method 3: MINITAB/BDMP
        r.kurt <- (r.kurt + 3) * (1 - 1/n)^2 - 3
    }
    r.kurt
  }
  
  if(is.na(conf.level)){
    res <- i.kurt(x, na.rm=na.rm, method=method)    
    
  } else {
  
    # Problematic standard errors and confidence intervals for kurtness and kurtosis.
    # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
    # adjusted bootstrap percentile (BCa) interval  
    boot.kurt <- boot(x, function(x, d) i.kurt(x[d], na.rm=na.rm, method=method), R=R)
    ci <- boot.ci(boot.kurt, conf=conf.level, type=type)
    res <- c(kurt=boot.kurt$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
  }

  return(res)
    
}
