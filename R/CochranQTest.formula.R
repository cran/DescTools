CochranQTest.formula <-
function(formula, data, subset, na.action, ...){
  res <- friedman.test(formula, data, subset, na.action, ...)
  attr(res$statistic, "names") <- "Q"
  res$method <- "Cochran's Q test"
  return(res)
}
