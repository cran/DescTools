CochranArmitageTest <-
function(x, alternative = c("two.sided","increasing","decreasing")) { 

  # based on: 
  # http://tolstoy.newcastle.edu.au/R/help/05/07/9442.html   
  DNAME <- deparse(substitute(x))
  
  if (!(any(dim(x)==2))) 
    stop("Cochran-Armitage test for trend must be used with rx2-table", call.=FALSE) 
      
  if (dim(x)[2]!=2) x <- t(x)          
  
  nidot <- apply(x, 1, sum)
  n <- sum(nidot)
  
  # Ri <- scores(x, 1, "table")
  Ri <- 1:dim(x)[1]
  Rbar <- sum(nidot*Ri)/n
  
  s2 <- sum(nidot*(Ri-Rbar)^2)
  pdot1 <- sum(x[,1])/n
  z <- sum(x[,1]*(Ri-Rbar))/sqrt(pdot1*(1-pdot1)*s2)
  STATISTIC <- z
  
  alternative <- match.arg(alternative)
  
  PVAL <- switch(alternative,
            two.sided = 2*pnorm(abs(z), lower.tail=FALSE),
            increasing = pnorm(z),
            decreasing = pnorm(z, lower.tail=FALSE) )
  
  PARAMETER <- dim(x)[1]
  names(STATISTIC) <- "Z"
  names(PARAMETER) <- "dim"
  
  METHOD <- "Cochran-Armitage test for trend"
  structure(list(statistic = STATISTIC, parameter = PARAMETER, alternative = alternative,
                 p.value = PVAL, method = METHOD, data.name = DNAME 
                 ), class = "htest")
      
}
