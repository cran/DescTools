MultinomCI <-
function(x, conf.level = 0.95, method = c("sisonglaz", "cplus1", "goodman")) {
  
  # Code mainly by:
  # Pablo J. Villacorta Iglesias <pjvi@decsai.ugr.es>\n
  # Department of Computer Science and Artificial Intelligence, University of Granada (Spain) 
  
  .moments <- function(c, lambda){
    
    a <- lambda + c
    b <- lambda - c
    if(b < 0) b <- 0
    if(b > 0) den <- ppois(a, lambda) - ppois(b-1, lambda)
    if(b == 0) den <- ppois(a,lambda)
    
    mu <- mat.or.vec(4,1) 
    mom <- mat.or.vec(5,1) 
    for(r in 1:4){
      poisA <- 0
      poisB <- 0
      
      if((a-r) >=0){ poisA <- ppois(a,lambda)-ppois(a-r,lambda) }
      if((a-r) < 0){ poisA <- ppois(a,lambda) }
      if((b-r-1) >=0){ poisB <- ppois(b-1,lambda)-ppois(b-r-1,lambda) }
      if((b-r-1) < 0 && (b-1)>=0){ poisB <- ppois(b-1,lambda) }
      if((b-r-1) < 0 && (b-1) < 0){ poisB <- 0 }
      
      mu[r] <- (lambda^r)*(1-(poisA-poisB)/den)
    }
    mom[1] <- mu[1]
    mom[2] <- mu[2] + mu[1] - mu[1]^2
    mom[3] <- mu[3] + mu[2]*(3-3*mu[1]) + (mu[1]-3*mu[1]^2+2*mu[1]^3)
    mom[4] <- mu[4] + mu[3]*(6-4*mu[1]) + mu[2]*(7-12*mu[1]+6*mu[1]^2)+mu[1]-4*mu[1]^2+6*mu[1]^3-3*mu[1]^4
    mom[5] <- den
    
    return(mom)
    
  }
  
  .truncpoi <- function(c, x, n, k){
    
    m <- matrix(0, k, 5)
    
    for(i in 1:k){
      lambda <- x[i]
      mom <- .moments(c, lambda)
      for(j in 1:5){ m[i,j] <- mom[j] }
    }
    for(i in 1:k){ m[i, 4] <- m[i, 4] - 3 * m[i, 2]^2 }
    
    s <- colSums(m)
    s1 <- s[1]
    s2 <- s[2]
    s3 <- s[3]
    s4 <- s[4]
    
    probn <- 1/(ppois(n,n)-ppois(n-1,n))
    z <- (n-s1)/sqrt(s2)
    g1 <- s3/(s2^(3/2))
    g2 <- s4/(s2^2)
    poly <- 1 + g1*(z^3-3*z)/6 + g2*(z^4-6*z^2+3)/24
    + g1^2*(z^6-15*z^4 + 45*z^2-15)/72
    f <- poly*exp(-z^2/2)/(sqrt(2)*gamma(0.5))
    
    probx <- 1
    for(i in 1:k){ probx <- probx * m[i,5]  }
    
    return(probn * probx * f / sqrt(s2))
  }
  
  
  n <- sum(x, na.rm=TRUE)
  k <- length(x)
  p <- x/n
  
  if (missing(method)) method <- "sisonglaz"
  
  method <- match.arg(arg = method, choices = c("sisonglaz", "cplus1", "goodman"))
  if(method == "goodman") {
           
    q.chi <- qchisq(conf.level, k - 1)
    lci <- (q.chi + 2*x - sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))
    uci <- (q.chi + 2*x + sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))
    
    res <- cbind(est=p, lwr.ci=lci, upr.ci=uci)
           
  } else {  # sisonglaz, cplus1

    const <- 0
    pold <- 0
    
    for(cc in 1:n){
      poi <- .truncpoi(cc, x, n, k)
      if(poi > conf.level && pold < conf.level) { 
        const <- cc
        break 
      }
      pold <- poi
    }

    delta <- (conf.level - pold)/(poi - pold)
    const <- const - 1
    
    if(method == "sisonglaz") {
      res <- cbind(est = p, lwr.ci = pmax(0, p - const/n), upr.ci = pmin(1, p + const/n + 2*delta/n))
      
    } else if(method == "cplus1") {
      res <- cbind(est = p, lwr.ci = pmax(0, p - const/n - 1/n), upr.ci = pmin(1,p + const/n + 1/n))
    }   
  }
   
  return(res)
}
