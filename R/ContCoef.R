ContCoef <-
function(x, y = NULL, correct = FALSE, ...) { 
  if(!is.null(y)) x <- table(x, y, ...)
  chisq <- suppressWarnings(chisq.test(x, correct = FALSE)$statistic)
  cc <- as.numeric( sqrt( chisq / ( chisq + sum(x)) ))
  if(correct) {  # Sakoda's adjusted Pearson's C
    k <- min(nrow(x),ncol(x)) 
    cc <- cc/sqrt((k-1)/k)
  }  
  return(cc)
}
