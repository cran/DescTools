StuartTauC <-
function(x, y = NULL, conf.level = NA, ...) {

  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)
  
  # Reference:
  # http://v8doc.sas.com/sashtml/stat/chap28/sect18.htm
  x <- ConDisPairs(tab)
  
  m <- min(dim(tab))
  n <- sum(tab)
  # Asymptotic standard error: sqrt(sigma2)
  sigma2 <- 4 * m^2 / ((m-1)^2 * n^4) * (sum(tab * (x$pi.c - x$pi.d)^2) - 4 * (x$C -x$D)^2/n)
  # debug: print(sqrt(sigma2))
  
  # Tau-c = (C - D)*[2m/(n2(m-1))] 
  tauc <- (x$C - x$D) * 2 * min(dim(tab)) / (sum(tab)^2*(min(dim(tab))-1))  
  
  if(is.na(conf.level)){
    result <- tauc
  } else {
    pr2 <- 1 - (1 - conf.level)/2
    CI <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + tauc
    result <- c(tauc = tauc,  lwr.ci=max(CI[1], -1), ups.ci=min(CI[2], 1))
  }               
  
  return(result)
  
}
