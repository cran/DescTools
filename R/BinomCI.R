BinomCI <-
function(x, n, conf.level = 0.95, method = c("wilson", "wald", "agresti-coull", "jeffreys", "modified wilson", 
    "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting"), rand = 123) {
  
  if(missing(method)) method <- "wilson"
  
  iBinomCI <- function(x, n, conf.level = 0.95, method = c("wilson", "wald", "agresti-coull", "jeffreys", "modified wilson", 
      "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting"), rand = 123) {
    
    if(length(x) != 1) stop("'x' has to be of length 1 (number of successes)")
    if(length(n) != 1) stop("'n' has to be of length 1 (number of trials)")
    if(length(conf.level) != 1)  stop("'conf.level' has to be of length 1 (confidence level)")
    if(conf.level < 0.5 | conf.level > 1)  stop("'conf.level' has to be in [0.5, 1]")
    
    alpha <- 1 - conf.level
    kappa <- qnorm(1-alpha/2)
    p.hat <- x/n
    q.hat <- 1 - p.hat
    
    switch( match.arg(arg=method, choices=c("wilson", "wald", "agresti-coull", "jeffreys", "modified wilson", 
         "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting"))
          , "wald" = { 
            est <- p.hat
            term2 <- kappa*sqrt(p.hat*q.hat)/sqrt(n)
            CI.lower <- max(0, p.hat - term2)
            CI.upper <- min(1, p.hat + term2)
            }
          , "wilson" = { 
            est <- p.hat
            term1 <- (x + kappa^2/2)/(n + kappa^2)
            term2 <- kappa*sqrt(n)/(n + kappa^2)*sqrt(p.hat*q.hat + kappa^2/(4*n))
            CI.lower <-  max(0, term1 - term2)
            CI.upper <- min(1, term1 + term2)
          }
          , "agresti-coull" = { 
            x.tilde <- x + kappa^2/2
            n.tilde <- n + kappa^2
            p.tilde <- x.tilde/n.tilde
            q.tilde <- 1 - p.tilde
            est <- p.tilde
            term2 <- kappa*sqrt(p.tilde*q.tilde)/sqrt(n.tilde)
            CI.lower <- max(0, p.tilde - term2)
            CI.upper <- min(1, p.tilde + term2)
          }
          , "jeffreys" = { 
            est <- p.hat
            if(x == 0)
              CI.lower <- 0
            else
              CI.lower <- qbeta(alpha/2, x+0.5, n-x+0.5)
            if(x == n)
              CI.upper <- 1
            else
              CI.upper <- qbeta(1-alpha/2, x+0.5, n-x+0.5)
          }
          , "modified wilson" = { 
            est <- p.hat
            term1 <- (x + kappa^2/2)/(n + kappa^2)
            term2 <- kappa*sqrt(n)/(n + kappa^2)*sqrt(p.hat*q.hat + kappa^2/(4*n))
            if((n <= 50 & x %in% c(1, 2)) | (n >= 51 & n <= 100 & x %in% c(1:3)))
              CI.lower <- 0.5*qchisq(alpha, 2*x)/n
            else
              CI.lower <-  max(0, term1 - term2)
            
            if((n <= 50 & x %in% c(n-1, n-2)) | (n >= 51 & n <= 100 & x %in% c(n-(1:3))))
              CI.upper <- 1 - 0.5*qchisq(alpha, 2*(n-x))/n
            else
              CI.upper <- min(1, term1 + term2)
          }
         , "modified jeffreys" = { 
           est <- p.hat
           if(x == n) 
             CI.lower <- (alpha/2)^(1/n)
           else {
             if(x <= 1)
               CI.lower <- 0
             else
               CI.lower <- qbeta(alpha/2, x+0.5, n-x+0.5)
           }
           if(x == 0)
             CI.upper <- 1 - (alpha/2)^(1/n)
           else{
             if(x >= n-1)
               CI.upper <- 1
             else
               CI.upper <- qbeta(1-alpha/2, x+0.5, n-x+0.5)
           }
         }
          , "clopper-pearson" = { 
            est <- p.hat
            CI.lower <- qbeta(alpha/2, x, n-x+1)
            CI.upper <- qbeta(1-alpha/2, x+1, n-x)
          }
          , "arcsine" = { 
            p.tilde <- (x + 0.375)/(n + 0.75)
            est <- p.tilde
            CI.lower <- sin(asin(sqrt(p.tilde)) - 0.5*kappa/sqrt(n))^2
            CI.upper <- sin(asin(sqrt(p.tilde)) + 0.5*kappa/sqrt(n))^2
          }
          , "logit" = { 
            est <- p.hat
            lambda.hat <- log(x/(n-x))
            V.hat <- n/(x*(n-x))
            lambda.lower <- lambda.hat - kappa*sqrt(V.hat)
            lambda.upper <- lambda.hat + kappa*sqrt(V.hat)
            CI.lower <- exp(lambda.lower)/(1 + exp(lambda.lower))
            CI.upper <- exp(lambda.upper)/(1 + exp(lambda.upper))
          }
          , "witting" = { 
            set.seed(rand)
            x.tilde <- x + runif(1, min = 0, max = 1)
            pbinom.abscont <- function(q, size, prob){
                v <- trunc(q)
                term1 <- pbinom(v-1, size = size, prob = prob) 
                term2 <- (q - v)*dbinom(v, size = size, prob = prob)
                return(term1 + term2)
            }
            qbinom.abscont <- function(p, size, x){
                fun <- function(prob, size, x, p){
                    pbinom.abscont(x, size, prob) - p
                }
                uniroot(fun, interval = c(0, 1), size = size, x = x, p = p)$root
            }
            est <- p.hat
            CI.lower <- qbinom.abscont(1-alpha, size = n, x = x.tilde)
            CI.upper <- qbinom.abscont(alpha, size = n, x = x.tilde)
          }
    )
    
    ci <- c( est=est, lwr.ci=CI.lower, upr.ci=CI.upper )
    return(ci)
    # original code:
    # CI <- c(CI.lower, CI.upper)
    # attr(CI, "confidence level") <- conf.level
    # return(list("estimate" = est, "CI" = CI))
  }  
  
  # handle vectors 
  # which parameter has the highest dimension
  lst <- list(x=x, n=n, conf.level=conf.level, method=method, rand=rand)
  maxdim <- max(unlist(lapply(lst, length)))
  # recycle all params to maxdim
  lgp <- lapply( lst, rep, length.out=maxdim )
  
  res <- sapply(1:maxdim, function(i) iBinomCI(x=lgp$x[i], n=lgp$n[i], conf.level=lgp$conf.level[i], method=lgp$method[i], rand=lgp$rand[i]))
  rownames(res)[1] <- c("est")
  colnames(res) <- names(x)
  
  return(t(res))
  
}
