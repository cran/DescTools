GoodmanKruskalGamma <-
function(x, y = NULL, conf.level = NA, ...) {
 
  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)

  # tab is a matrix of counts   
  # Based on code of Michael Friendly and Laura Thompson
  # Confidence interval calculation and output from Greg Rodd
  
  x <- ConDisPairs(tab)
  
  psi <- 2 * (x$D * x$pi.c - x$C * x$pi.d)/(x$C + x$D)^2
  # Asymptotic standard error: sqrt(sigma2)
  sigma2 <- sum(tab * psi^2) - sum(tab * psi)^2
  
  gamma <- (x$C - x$D)/(x$C + x$D)
  
  if(is.na(conf.level)){
    result <- gamma
  } else {
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + gamma
    result <- c(gamma = gamma,  lwr.ci=max(ci[1], -1), ups.ci=min(ci[2], 1))
  }               
  
  return(result)
  
}
