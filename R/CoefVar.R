CoefVar <-
function(x, unbiased = FALSE, conf.level = NA, na.rm = FALSE) {
  
  if(na.rm) x <- na.omit(x)
  
  res <- sd(x) / mean(x)
  n <- length(x)
  if(unbiased) {
    res <- res * ((1 - (1/(4*(n-1))) + (1/n) * res^2)+(1/(2*(n-1)^2)))
  }
  
  
  if(!is.na(conf.level)){
    ci <- .nctCI(sqrt(n)/res, df = n-1, conf = conf.level) 
    res <- c(est=res, low.ci= unname(sqrt(n)/ci["upr.ci"]), upr.ci= unname(sqrt(n)/ci["lwr.ci"]))
  }
  
  return(res)
  
}
