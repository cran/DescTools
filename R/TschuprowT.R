TschuprowT <-
function(x, y = NULL, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  # Tschuprow, A. A. (1939) Principles of the Mathematical Theory of Correlation; translated by M. Kantorowitsch. W. Hodge & Co.
  # http://en.wikipedia.org/wiki/Tschuprow's_T
  # Hartung S. 451

  # what can go wrong while calculating chisq.stat?
  # we don't need test results here, so we suppress those warnings
  as.numeric( sqrt(suppressWarnings(chisq.test(x, correct = FALSE)$statistic)/
                  (sum(x) * sqrt(prod(dim(x)-1)) )))

}
