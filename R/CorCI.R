CorCI <-
function(rho, n, conf.level = 0.95, twotailed=TRUE) {
  
    z <- FisherZ(rho)
    if(n<4) {stop("number of subjects must be greater than 3")}
    se <- 1/sqrt(n-3)
    conf.level <- 1-conf.level 
    if(twotailed) conf.level<- conf.level/2
    dif <- qnorm(conf.level)
    zlow <- z + dif*se
    zhigh <- z - dif*se
    ci <- c(zlow,zhigh)
    ci <- FisherZInv(ci)
    
    return(c(cor = rho, lwr.ci = ci[1], upr.ci = ci[2]))
}
