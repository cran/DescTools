RunsTest.default <-
function(x, y=NULL, alternative=c("two.sided", "less", "greater"), exact=NULL, na.rm = FALSE, ...) {

  # exact values:
  # http://www.reiter1.com/Glossar/Wald_Wolfowitz.htm
  
  # example:   x <- sample(c(0,1), size=20, r=TRUE)

  pruns <- function(r, n1, n2, alternative=c("two.sided", "less", "greater")) {

    # source: randomizeBE
    # author: D. Labes <detlewlabes at gmx.de>
    
    # function for calculating the denominator of the runs distribution
    .druns_nom <- function(r, n1, n2){
      pp <- vector(mode="numeric",length=length(r))
      for (i in seq_along(r)){
        if (2*r[i]%/%2==r[i]){
          # even 2*k
          k <- r[i]/2
          pp[i] <- 2*choose(n1-1, k-1)*choose(n2-1, k-1)
        } else {
          # odd 2*k+1
          k <- (r[i]-1)/2
          pp[i] <- choose(n1-1,k-1) * choose(n2-1,k) +
            choose(n1-1,k)   * choose(n2-1,k-1)
        }
      }  
      pp
    }
    
    alternative <- match.arg(alternative)
    
    n <- n1+n2
    
    if(r<=1) stop("Number of runs must be > 1")
    if(r>n) stop("Number of runs must be < (n1+n2")
    if(n1<1 | n2<1) return(0) #??? is not random!
    
    E <- 1 + 2*n1*n2/n
    
    denom <- choose(n,n1)
    # how long should we make the r vector?
    # in very unsymmetric cases only a few elements of
    # pp = density have values > 0 if rmax=n1+n2
    # number of runs possible: 2*m if n=m, 2*m+1 if m<n
    rmax <- ifelse(n1==n2, 2*n1, 2*min(n1,n2)+1)
    rv <- 2:rmax
    pp <- .druns_nom(rv, n1, n2)
    
    # pL is p(R<=r) -> left/lower tail
    pL <- sum(pp[rv<=r])/denom 
    #pU is p(R>=r) -> right/upper tail
    pU <- 1 - sum(pp[rv<=(r-1)])/denom
    
    # Equn. 4.7 of the SPSS documentation
    p2 <- sum(pp[abs(rv-E)>=abs(r-E)])/denom
    
    # Next is the rule from:
    # Gibbons "Nonparametric Methods for Quantitative Analysis"
    # 0.5 is to avoid p>1 if both pL and pU are >0.5
    p2min <- 2*min(c(pL, pU, 0.5))
    
    # we are using the SPSS approach wich takes into account the 
    # unsymmetric form of the distribution if n1 << n2
    
    return(
      switch( alternative
              , "less" = pL 
              , "greater" = pU
              , "two.sided" = p2 
      )  
    )  
    
  }
  
  
  if(!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    # perform Wald-Wolfowitz-Test with 2 variables
    xy <- Sort(cbind(c(x,y), c(rep(0, length(x)), rep(1, length(y)))))[,2]
    res <- RunsTest(x=xy, alternative=alternative, exact=exact, na.rm=na.rm)
    res$data.name <- dname
    res$method <- "Wald-Wolfowitz Runs Test "
    return(res)
  }
  
  alternative <- match.arg(alternative)
  dname <- deparse(substitute(x))
  
  # Strip NAs
  if (na.rm) x <- na.omit(x)

  # let's have a 0,1 vector if x is a numeric vector with more than 2 values
  if(is.numeric(x) & (length(unique(x))>2)) { 
    est <- median(x, na.rm=TRUE)
    names(est) <- "median(x)"
    x <- ((x > est)*1)
    
  } else {
    est <- NULL
  } 

  x <- factor(x)
  if( nlevels(x) %nin% c(1,2) ) stop("Can only process dichotomous variables")
  x <- as.numeric(x) - 1
  
  # x <- sample(c(0,1), 100000000, replace=TRUE)
  # ### user  system elapsed 
  #   9.39    6.76   16.30    system.time(rle(x))
  #   4.49    3.45    8.00    system.time(sum(diff(x) != 0) + 1)
  # so don't use rle...
  
  runs <- sum(diff(x) != 0) + 1
  m <- sum(x==0)
  n <- sum(x==1)
  
  if(is.null(exact)) { exact <- ((m +n) <= 30) }
  
  E <- 1 + 2*n*m / (n + m)
  s2 <- (2*n*m * (2*n*m - n - m)) / ((n + m)^2 * (n + m - 1))
  
  # this is the SPSS-Definition
  # http://publib.boulder.ibm.com/infocenter/spssstat/v20r0m0/index.jsp?topic=%2Fcom.ibm.spss.statistics.help%2Fidh_idd_npar_onesample_settings_tests_runs.htm
  if( n+m >= 50) {
    statistic <- (runs - E) / sqrt(s2)
  } else { 
    switch( as.character(cut(runs - E, breaks=c(-Inf, -0.5, 0.5, Inf), labels=c("a", "b", "c")))
      , "a" = statistic <- (runs - E + 0.5) / sqrt(s2)
      , "b" = statistic <- 0 
      , "c" = statistic <- (runs - E - 0.5) / sqrt(s2) 
    )
  }  
  
  switch( alternative
    , "less" = { 
        p.value <- ifelse(exact, pruns(runs, m, n, alternative="less"), pnorm(statistic) )
        alternative <- "true number of runs is less than expected"
      }
    , "greater" = { 
        p.value = ifelse(exact, pruns(runs, m, n, alternative="greater"), 1 - pnorm(statistic) )
        alternative <- "true number of runs is greater than expected"
      }
    , "two.sided" = { 
        p.value = ifelse(exact, pruns(runs, m, n, alternative="two.sided"), 
                         2 * min(pnorm(statistic), 1 - pnorm(statistic)) )
        alternative <- "true number of runs is not equal the expected number"
      } 
  )  

  method = "Runs Test for Randomness"
  names(statistic) <- "z"  # Standardized Runs Statistic
  
  # do not report statistic when exact p-value is calculated
  if(exact) statistic <- NULL
  
  structure(list(
    statistic = statistic, 
    p.value = p.value,
    method = method,
    alternative = alternative,    
    data.name = dname,
    estimate = est,
    parameter = c(runs=runs, m=m, n=n)), 
  class = "htest")

}
