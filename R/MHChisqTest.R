MHChisqTest <-
function(x, srow=1:nrow(x), scol=1:ncol(x)){
  
  # calculates Mantel-Haenszel Chisquare test
  
  DNAME <- deparse(substitute(x))
  
  STATISTIC <- (sum(x) - 1) * corr(d=GetPairs(srow, scol), as.vector(x))^2
  PARAMETER <- 1
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  METHOD <- "Mantel-Haenszel Chi-Square"
  
  structure(list(statistic = STATISTIC, parameter = PARAMETER, 
                 p.value = PVAL, method = METHOD, data.name = DNAME), class = "htest")
}
