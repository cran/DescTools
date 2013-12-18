CochranQTest.default <-
function(y, groups, blocks, ...){
  res <- friedman.test(y, groups, blocks, ...)
  attr(res$statistic, "names") <- "Q"
  res$method <- "Cochran's Q test"
  return(res)
}
