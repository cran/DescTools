CochranQTest <-
function(y, ...){
  
  # Cochran's Q Test is analogue to the friedman.test with 0,1 coded response
  
  res <- friedman.test(y, ...)
  attr(res$statistic, "names") <- "Q"
  res$method <- "Cochran's Q test"
  return(res)
}
