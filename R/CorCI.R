CorCI <-
function(rho, n, conf.level = 0.95, alternative = c("two.sided","less","greater")) {
  
    
    if (n < 3L) 
      stop("not enough finite observations")
    
    if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) 
                                 || conf.level < 0 || conf.level > 1)) 
      stop("'conf.level' must be a single number between 0 and 1")
    
    alternative <- match.arg(alternative)
    
    z <- FisherZ(rho)
    sigma <- 1/sqrt(n - 3)
    
    ci <- switch(alternative, 
                 less = c(-Inf, z + sigma * qnorm(conf.level)), 
                 greater = c(z - sigma * qnorm(conf.level), Inf), 
                 two.sided = z + c(-1, 1) * sigma * qnorm((1 + conf.level)/2))
    ci <- FisherZInv(ci)
    
    return(c(cor = rho, lwr.ci = ci[1], upr.ci = ci[2]))
}
