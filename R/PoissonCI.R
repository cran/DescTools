PoissonCI <-
function(x, conf.level = 0.95, method = c("score", "wald", "agresti-coull", "garwood"), na.rm = FALSE) {
  
  # ref:  http://www.ijmo.org/papers/189-S083.pdf but wacklig!!!
  # http://www.math.montana.edu/~rjboik/classes/502/ci.pdf
  # http://www.ine.pt/revstat/pdf/rs120203.pdf
  
  # see also:   pois.conf.int {epitools}
  
  if(na.rm) x <- na.omit(x)
  if(missing(method)) method <- "score"
  
  if(length(conf.level) != 1)  stop("'conf.level' has to be of length 1 (confidence level)")
  if(conf.level < 0.5 | conf.level > 1)  stop("'conf.level' has to be in [0.5, 1]")
  
  alpha <- 1 - conf.level
  z <- qnorm(1-alpha/2)

  n <- length(x)
  est <- mean(x, na.rm = na.rm)
  
  switch( match.arg(arg=method, choices=c("score", "wald", "agresti-coull","garwood"))
        , "score" = { 
          term1 <- (est+ z^2/(2*n)) 
          term2 <- z / sqrt(n) * sqrt(est + z^2/(4*n))
          lwr.ci <- max(0, term1 - term2)
          upr.ci <- min(1, term2 + term2)
          }
        , "wald" = { 
          term2 <- z*sqrt(est/n)
          lwr.ci <- 0
          upr.ci <- 1
          }
        , "agresti-coull" = { 
          lwr.ci <- 0
          upr.ci <- 1
        }
        , "garwood" = { 
          lwr.ci <- qchisq((1 - conf.level)/2, 2 * length(x))/2
          upr.ci <- qchisq(1 - (1 - conf.level)/2, 2 * (length(x) + 1))/2
        }
  )
  
  ci <- c( est=est, lwr.ci=lwr.ci, upr.ci=upr.ci )
  return(ci)

}
