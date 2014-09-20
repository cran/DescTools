SomersDelta <-
function(x,  y = NULL, direction=c("row","column"), conf.level = NA, ...) {

  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)
  
  # tab is a matrix of counts   
  x <- ConDisPairs(tab)

# use .DoCount
#   if(is.na(conf.level)) {
#     d.tab <- as.data.frame.table(tab)
#     x <- .DoCount(d.tab[,1], d.tab[,2], d.tab[,3])
#   } else {  
#     x <- ConDisPairs(tab)
#   }
  
  m <- min(dim(tab))
  n <- sum(tab)
  switch( match.arg( arg = direction, choices = c("row","column") )
    , "row" = { ni. <- apply(tab, 2, sum) }
    , "column" = { ni. <- apply(tab, 1, sum) }
  )
  wt <- n^2 - sum(ni.^2)
  # Asymptotic standard error: sqrt(sigma2)
  sigma2 <- 4/wt^4 * (sum(tab * (wt*(x$pi.c - x$pi.d) - 2*(x$C-x$D)*(n-ni.))^2))
  # debug: print(sqrt(sigma2))
  
  somers <- (x$C - x$D) / (n * (n-1) /2 - sum(ni. * (ni. - 1) /2 ))
  
  pr2 <- 1 - (1 - conf.level)/2
  ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + somers
  
  if(is.na(conf.level)){
    result <- somers
  } else {
    result <- c(somers = somers,  lwr.ci=max(ci[1], -1), ups.ci=min(ci[2], 1))
  }               
  
  return(result)
  
}
