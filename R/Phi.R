Phi <-
function (x, y = NULL, ...) { 
  if(!is.null(y)) x <- table(x, y, ...)
  # when computing phi, note that Yates' correction to chi-square must not be used.
  as.numeric( sqrt( suppressWarnings(chisq.test(x, correct=FALSE)$statistic) / sum(x) ) )
}
