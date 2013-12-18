UncertCoef <-
function(x, y = NULL, direction = c("symmetric", "row", "column"), 
                             conf.level = NA, p.zero.correction = 1/sum(x)^2, ... ) {
  # Theil's UC (1970)
  # slightly nudge zero values so that their logarithm can be calculated (cf. Theil 1970: x->0 => xlogx->0)
  if(!is.null(y)) x <- table(x, y, ...)

  x[x == 0] <- p.zero.correction
  
  n <- sum(x)
  rsum <- apply(x, 1, sum)
  csum <- apply(x, 2, sum)
  
  hx <- -sum((apply(x, 1, sum) * log(apply(x, 1, sum)/n))/n)
  hy <- -sum((apply(x, 2, sum) * log(apply(x, 2, sum)/n))/n)
  hxy <- -sum(apply(x, c(1, 2), sum) * log(apply(x, c(1, 2), sum)/n)/n)
  
  switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
          , "symmetric" = { res <- 2 * (hx + hy - hxy)/(hx + hy) }
          , "row" = { res <- (hx + hy - hxy)/hx }
          , "column" = { res <- (hx + hy - hxy)/hy }
  )
  
  if(!is.na(conf.level)){
    var.uc.RC <- var.uc.CR <- 0
    for(i in 1:nrow(x))
      for(j in 1:ncol(x))
      { var.uc.RC <- var.uc.RC + x[i,j]*(hx*log(x[i,j]/csum[j])+((hy-hxy)*log(rsum[i]/n)))^2/(n^2*hx^4);
        var.uc.CR <- var.uc.CR + x[i,j]*(hy*log(x[i,j]/rsum[i])+((hx-hxy)*log(csum[j]/n)))^2/(n^2*hy^4);
      }
    switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
            , "symmetric" = { 
              sigma2 <- 4*sum(x * (hxy * log(rsum %o% csum/n^2) - (hx+hy)*log(x/n))^2 ) / 
                (n^2*(hx+hy)^4) 
            }
            , "row" = { sigma2 <- var.uc.RC }
            , "column" = { sigma2 <- var.uc.CR }
    )
    
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + res
    
    res <- c(uc = res,  lwr.ci=max(ci[1], -1), ups.ci=min(ci[2], 1))
  }               
  return(res)
}
