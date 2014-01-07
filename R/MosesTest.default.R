MosesTest.default <-
function(x, y, extreme = NULL, ...){

  # example
  # x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
  # y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
  # MosesTest(y, x)

  if(is.null(extreme)) extreme <- pmax(floor(0.05 * length(x)), 1)
  h <- extreme
  if(2*h > length(x)-2) h <- floor((length(x)-2)/2)
  
  # no alternative for the moses.test
  DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  
  nk <- length(x)
  ne <- length(y)
  # decreasing ranks following SPSS-calculation
  R1 <- rank(-c(x, y))[1:nk]
  R1 <- sort(R1)[(h+1):(length(R1)-h)]
  
  S <- ceiling(max(R1) - min(R1) + 1)
  
  tmp <- 0
  for( i in 0 : (S - nk + 2*h)) {
    tmp <- tmp + choose(i + nk - 2*h - 2, i) * choose(ne + 2*h + 1 - i, ne - i)
  }
  
  PVAL <- (tmp / choose(nk + ne, nk))

  structure(list(statistic = c(S = S),
                 p.value = PVAL,
                 method = "Moses Test of Extreme Reactions",
                 alternative = "extreme values are more likely in x than in y",
                 data.name = DNAME),
            class = "htest")
  
}
