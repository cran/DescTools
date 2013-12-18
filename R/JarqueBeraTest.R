JarqueBeraTest <-
function(x) {

  # Author: Adrian Trapletti
  
  if(NCOL(x) > 1)
      stop("x is not a vector or univariate time series")
  if(any(is.na(x)))
      stop("NAs in x")
  DNAME <- deparse(substitute(x))
  n <- length(x)
  m1 <- sum(x)/n
  m2 <- sum((x-m1)^2)/n
  m3 <- sum((x-m1)^3)/n
  m4 <- sum((x-m1)^4)/n
  b1 <- (m3/m2^(3/2))^2
  b2 <- (m4/m2^2)
  STATISTIC <- n*b1/6+n*(b2-3)^2/24
  names(STATISTIC) <- "X-squared"
  PARAMETER <- 2
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC,df = 2)
  METHOD <- "Jarque Bera Test"
  structure(list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME),
            class = "htest")
}
