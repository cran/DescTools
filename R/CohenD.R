CohenD <-
function(x, y=NULL, pooled = FALSE, correct = FALSE, conf.level = NA, na.rm = FALSE) {
  
  if (na.rm) {
    x <- na.omit(x)
    if(!is.null(y)) y <- na.omit(y)
  }  

  if(is.null(y)){   # one sample Cohen d
    d <- mean(x) / sd(x)
    n <- length(x)
    if(!is.na(conf.level)){
      # reference: Smithson Confidence Intervals pp. 36:
      ci <- .nctCI(d / sqrt(n), df = n-1, conf = conf.level)
      res <- c(d=d, lwr.ci=ci[1]/sqrt(n), upr.ci=ci[3]/sqrt(n))
    } else {
      res <- d
    }
  } else {
  
    meanx <- mean(x)
    meany <- mean(y)
    ssqx <- sum((x - meanx)^2)
    ssqy <- sum((y - meany)^2)
    nx <- length(x)
    ny <- length(y)
    
    DF <- nx + ny - 2
    d <- (meanx - meany) 
    
    if(!pooled){
      d <- d / sqrt(((nx - 1) * ssqx + (ny - 1) * ssqy) / DF)
    }else{
      d <- d / sd(d)
    }
    
    #  if(unbiased) d <- d * gamma(DF/2)/(sqrt(DF/2) * gamma((DF - 1)/2))
    
    if(correct){  # "Hedges's g"
      # Hedges, L. V. & Olkin, I. (1985). Statistical methods for meta-analysis. Orlando, FL: Academic Press.
      d <- d * (1 - 3 / ( 4 * (nx + ny) - 9))
    }  
    
    if(!is.na(conf.level)) {
      # old:
      # The Handbook of Research Synthesis and Meta-Analysis (Cooper, Hedges, & Valentine, 2009)
      ## p 238
      # ci <- d + c(-1, 1) * sqrt(((nx+ny) / (nx*ny) + .5 * d^2 / DF) * ((nx + ny)/DF)) * qt((1 - conf.level) / 2, DF)
 
      # supposed to be better, Smithson's version:
      ci <- .nctCI(d / sqrt(nx*ny/(nx+ny)), df = DF, conf = conf.level)
      res <- c(d=d, lwr.ci=ci[1]/sqrt(nx*ny/(nx+ny)), upr.ci=ci[3]/sqrt(nx*ny/(nx+ny)))
      
      res <- c(d=d, lwr.ci=ci[1], upr.ci=ci[2])
    } else {
      res <- d
    }
  }
  
  ## Cohen, J. (1992). A power primer. Psychological Bulletin, 112, 155-159. Crow, E. L. (1991).
  attr(res, "magnitude") <- c("negligible","small","medium","large")[findInterval(abs(d), c(0.2, 0.5, 0.8)) + 1]
  
  return(res)
  
}
