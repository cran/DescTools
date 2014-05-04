PoissonCI <-
function(x, n = 1, conf.level = 0.95, 
                      method = c("exact","score", "wald")) {
  
  iPoissonCI <- function(x, n = 1, conf.level = 0.95, 
                      method = c("exact","score", "wald")) {
  
    # ref:  http://www.ijmo.org/papers/189-S083.pdf but wacklig!!!
    # http://www.math.montana.edu/~rjboik/classes/502/ci.pdf
    # http://www.ine.pt/revstat/pdf/rs120203.pdf
    # http://www.pvamu.edu/include/Math/AAM/AAM%20Vol%206,%20Issue%201%20(June%202011)/06_%20Kibria_AAM_R308_BK_090110_Vol_6_Issue_1.pdf
    
    # see also:   pois.conf.int {epitools}
    
    if(missing(method)) method <- "score"
    
    if(length(conf.level) != 1)  stop("'conf.level' has to be of length 1 (confidence level)")
    if(conf.level < 0.5 | conf.level > 1)  stop("'conf.level' has to be in [0.5, 1]")
    
    alpha <- 1 - conf.level
    z <- qnorm(1-alpha/2)
  
    lambda <- x/n
    
    switch( match.arg(arg=method, choices=c("exact","score", "wald"))
            , "exact" = {
              ci <- poisson.test(x, n, conf.level = conf.level)$conf.int
              lwr.ci <- ci[1]
              upr.ci <- ci[2]
            }
            , "score" = { 
              term1 <- (x + z^2/2)/n 
              term2 <- z * n^-0.5 * sqrt(x/n + z^2/(4*n))
              lwr.ci <- term1 - term2
              upr.ci <- term1 + term2
            }
            , "wald" = { 
              term2 <- z*sqrt(lambda/n)
              lwr.ci <- lambda - term2
              upr.ci <- lambda + term2
            }
  # agresti-coull is the same as score
  #             , "agresti-coull" = {
  #               lwr.ci <- lambda + z^2/(2*n) - z*sqrt(lambda/n + z^2/(4*n^2))
  #               upr.ci <- lambda + z^2/(2*n) + z*sqrt(lambda/n + z^2/(4*n^2))
  #               
  #             }
  # garwood is the same as exact, check that!!
  #             , "garwood" = { 
  #               lwr.ci <- qchisq((1 - conf.level)/2, 2*x)/(2*n)
  #               upr.ci <- qchisq(1 - (1 - conf.level)/2, 2*(x + 1))/(2*n)
  #             }
    )
    
    ci <- c( est=lambda, lwr.ci=lwr.ci, upr.ci=upr.ci )
    return(ci)
  }

  # handle vectors 
  # which parameter has the highest dimension
  lst <- list(x=x, n=n, conf.level=conf.level, method=method)
  maxdim <- max(unlist(lapply(lst, length)))
  # recycle all params to maxdim
  lgp <- lapply( lst, rep, length.out=maxdim )
  
  res <- sapply(1:maxdim, function(i) iPoissonCI(x=lgp$x[i], n=lgp$n[i], conf.level=lgp$conf.level[i], method=lgp$method[i]))
  rownames(res)[1] <- c("est")
  colnames(res) <- names(x)
  
  return(t(res))

}
