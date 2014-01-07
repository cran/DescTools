GoodmanKruskalTauA <-
function(x, y = NULL, direction = c("row", "column"), conf.level = NA, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  n <- sum(x)
  n.err.unconditional <- n^2
  sum.row <- rowSums(x)
  sum.col <- colSums(x)
  
  switch( match.arg( arg = direction, choices = c("row", "column") )
          , "column" = {             # Tau Column|Row
            
            for(i in 1:nrow(x))
              n.err.unconditional <- n.err.unconditional-n*sum(x[i,]^2/sum.row[i])
            n.err.conditional <- n^2-sum(sum.col^2)
            tau.CR <- 1-(n.err.unconditional/n.err.conditional)
            v <- n.err.unconditional/(n^2)
            d <- n.err.conditional/(n^2)
            f <- d*(v+1)-2*v
            var.tau.CR <- 0
            for(i in 1:nrow(x))
              for(j in 1:ncol(x))
                var.tau.CR <- var.tau.CR + x[i,j]*(-2*v*(sum.col[j]/n)+d*((2*x[i,j]/sum.row[i])-sum((x[i,]/sum.row[i])^2))-f)^2/(n^2*d^4)
            ASE.tau.CR <- sqrt(var.tau.CR)
            est <- tau.CR
            sigma2 <- ASE.tau.CR^2
          }
          , "row" = {             # Tau Row|Column
            
            for(j in 1:ncol(x))
              n.err.unconditional <- n.err.unconditional-n*sum(x[,j]^2/sum.col[j])
            n.err.conditional <- n^2-sum(sum.row^2)
            tau.RC <- 1-(n.err.unconditional/n.err.conditional)
            v <- n.err.unconditional/(n^2)
            d <- n.err.conditional/(n^2)
            f <- d*(v+1)-2*v
            var.tau.RC <- 0
            for(i in 1:nrow(x))
              for(j in 1:ncol(x))
                var.tau.RC <- var.tau.RC + x[i,j]*(-2*v*(sum.row[i]/n)+d*((2*x[i,j]/sum.col[j])-sum((x[,j]/sum.col[j])^2))-f)^2/(n^2*d^4)
            ASE.tau.RC <- sqrt(var.tau.RC)
            est <- tau.RC
            sigma2 <- ASE.tau.RC^2
          }
  )
  
  if(is.na(conf.level)){
    res <- est
  } else {
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + est
    res <- c(tauA=est, lwr.ci=ci[1], upr.ci=ci[2])
  }  
  
  return(res)
}
