RunsTest <-
function(x, alternative=c("two.sided", "less", "greater"), na.rm = FALSE) {

  # example:   x <- sample(c(0,1), size=20, r=TRUE)

  alternative <- match.arg(alternative)
  dname <- deparse(substitute(x))

  # Strip NAs
  if (na.rm) x <- na.omit(x)

  # let's have a 0,1 vector
  if(is.numeric(x)) { 
    est <- median(x, na.rm=TRUE)
    names(est) <- "median(x)"
    x <- ((x > est)*1)
    
  } else {
    est <- NULL
  } 
  x <- factor(x)
  if( nlevels(x) %nin% c(1,2) ) stop("!RunsTest!: Can only process dichotomous variables")
  x <- as.numeric(x) - 1
  
  # x <- sample(c(0,1), 100000000, replace=TRUE)
  # ### user  system elapsed 
  #   9.39    6.76   16.30    system.time(rle(x))
  #   4.49    3.45    8.00    system.time(sum(diff(x) != 0) + 1)
  # so don't use rle...
  
  runs <- sum(diff(x) != 0) + 1
  m <- sum(x==0)
  n <- sum(x==1)
  
  E <- 1 + 2*n*m / (n + m)
  s2 <- (2*n*m * (2*n*m - n - m)) / ((n + m)^2 * (n + m - 1))
  
  # this is the SPSS-Definition
  # http://publib.boulder.ibm.com/infocenter/spssstat/v20r0m0/index.jsp?topic=%2Fcom.ibm.spss.statistics.help%2Fidh_idd_npar_onesample_settings_tests_runs.htm
  if( n+m >= 50) {
    statistic <- (runs - E) / sqrt(s2)
  } else { 
    switch( cut(runs - E, breaks=c(-Inf, -0.5, 0.5, Inf), labels=c("a", "b", "c"))
      , "a" = statistic <- (runs - E + 0.5) / sqrt(s2)
      , "b" = statistic <- 0 
      , "c" = statistic <- (runs - E - 0.5) / sqrt(s2) 
    )
  }  
  
  switch( alternative
    , "less" = { 
        p.value <- pnorm(statistic) 
        alternative <- "true number of runs is less than expected"
      }
    , "greater" = { 
        p.value = 1 - pnorm(statistic) 
        alternative <- "true number of runs is greater than expected"
      }
    , "two.sided" = { 
        p.value = 2 * min(pnorm(statistic), 1 - pnorm(statistic)) 
        alternative <- "true number of runs is not equal the expected number"
      } 
  )  

  method = "Runs Test for Randomness"
  names(statistic) <- "Standardized Runs Statistic"
  
  structure(list(
    statistic = statistic, 
    p.value = p.value,
    method = method,
    alternative = alternative,    
    data.name = dname,
    estimate = est,
    runs = runs,
    n = c(m,n)), 
  class = "htest")

}
