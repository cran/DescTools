CramerV <-
function(x, y = NULL, conf.level = NA, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  # CIs and power for the noncentral chi-sq noncentrality parameter (ncp):
  # The function lochi computes the lower CI limit and hichi computes the upper limit.
  # Both functions take 3 arguments: observed chi-sq, df, and confidence level.
  
  # author:   Michael Smithson
  # http://psychology3.anu.edu.au/people/smithson/details/CIstuff/Splusnonc.pdf
  
  lochi <- function(chival, df, conf) {
    ulim <- 1 - (1-conf)/2
    #  This first part finds a lower value from which to start.
    lc <- c(.001,chival/2,chival)
    while(pchisq(chival,df,lc[1])<ulim) {
      if(pchisq(chival,df)<ulim)
        return(c(0,pchisq(chival,df)))
      lc <- c(lc[1]/4,lc[1],lc[3])
    }
    #	This next part finds the lower limit for the ncp.
    diff <- 1
    while(diff > .00001) {
      if(pchisq(chival, df, lc[2]) < ulim) 
        lc <- c(lc[1],(lc[1]+lc[2])/2,lc[2]) 
      else lc <- c(lc[2],(lc[2]+lc[3])/2,lc[3])
      diff <- abs(pchisq(chival,df,lc[2]) - ulim)
      ucdf <- pchisq(chival,df,lc[2])
    }
    c(lc[2], ucdf)
  }
  
  hichi <- function(chival,df,conf) {
    #	This first part finds upper and lower startinig values.
    uc <- c(chival, 2*chival, 3*chival)
    llim <- (1-conf)/2
    while(pchisq(chival, df, uc[1]) < llim) {
      uc <- c(uc[1]/4,uc[1],uc[3])
    }
    while(pchisq(chival,df,uc[3])>llim) {
      uc <- c(uc[1],uc[3],uc[3]+chival)
    }
    #	This next part finds the upper limit for the ncp.
    diff <- 1
    while(diff > .00001) {
      if(pchisq(chival,df,uc[2]) < llim) 
        uc <- c(uc[1],(uc[1]+uc[2])/2,uc[2]) 
      else uc <- c(uc[2],(uc[2]+uc[3])/2,uc[3])
      diff <- abs(pchisq(chival,df,uc[2]) - llim)
      lcdf <- pchisq(chival,df,uc[2])
    }
    c(uc[2], lcdf)
  }
  
  # Remark Andri 18.12.2014:
  # lochi and hichi could be replaced with:
  #   optimize(function(x) abs(pchisq(chival, DF, x)  - (1-(1-conf.level)/2)), c(0, chival))
  #   optimize(function(x) abs(pchisq(chival, DF, x)  - (1-conf.level)/2), c(0, 3*chival))
  #
  # ... which would run ~ 25% faster and be more exact  
  
  # what can go wrong while calculating chisq.stat?
  # we don't need test results here, so we suppress those warnings
  chisq.hat <- suppressWarnings(chisq.test(x, correct = FALSE)$statistic)
  df <- prod(dim(x)-1)
  v <- as.numeric(sqrt(chisq.hat/(sum(x) * (min(dim(x)) - 1))))

  if (is.na(conf.level)) {
    res <- v  

  } else {  
    ci <- c(lochi(chisq.hat, df, conf.level)[1], hichi(chisq.hat, df, conf.level)[1]) 
# corrected by michael smithson, 17.5.2014:
#    ci <- unname(sqrt( (ci + df) / (sum(x) * (min(dim(x)) - 1)) ))
    ci <- unname(sqrt( (ci) / (sum(x) * (min(dim(x)) - 1)) ))
    res <- c("Cramer V"=v, lwr.ci=max(0, ci[1]), upr.ci=min(1, ci[2]))

  }    

  return(res)
}
