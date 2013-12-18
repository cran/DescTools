Lambda <-
function(x, y = NULL, direction = c("symmetric", "row", "column"), conf.level = NA, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  # Guttman'a lambda (1941), resp. Goodman Kruskal's Lambda (1954)

  n <- sum(x)
  csum <- colSums(x)
  rsum <- rowSums(x)
  rmax <- apply(x, 1, max)
  cmax <- apply(x, 2, max)
  max.rsum <- max(rsum)
  max.csum <- max(csum)
  
  nr <- nrow(x)
  nc <- ncol(x)
  
  switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
          , "symmetric" = { res <- 0.5*(sum(rmax, cmax) - (max.csum +  max.rsum)) / (n - 0.5*(max.csum +  max.rsum)) }
          , "column" = { res <- (sum(rmax) - max.csum) / (n - max.csum) }
          , "row" = { res <- (sum(cmax) - max.rsum) / (n - max.rsum) }
  )
  
  if(is.na(conf.level)){
    res <- res
  } else {

    L.col <- matrix(,nc)
    L.row <- matrix(,nr)
    
    switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
            , "symmetric" = { 

#     How to see:
#     http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#     pp. 1744              
#     Author:   Nina
              
              l <- which.max(csum)
              k <- which.max(rsum)
              li <- apply(x,1,which.max)
              ki <- apply(x,2,which.max)
              
              w <- 2*n-max.csum-max.rsum
              v <- 2*n -sum(rmax,cmax)
              xx <- sum(rmax[li==l], cmax[ki==k], rmax[k], cmax[l])
              y <- 8*n-w-v-2*xx
              
              t <- rep(NA, length(li))
              for (i in 1:length(li)){
                t[i] <- (ki[li[i]]==i & li[ki[li[i]]]==li[i]) 
              }                                    
              
              sigma2 <- 1/w^4*(w*v*y-2 *w^2*(n - sum(rmax[t]))-2*v^2*(n-x[k,l]))
              
            }
            , "column" = {
              L.col.max <- min(which(csum == max.csum))
              for(i in 1:nr) {
                if(length(which(x[i, intersect(which(x[i,] == max.csum), which(x[i,] == max.rsum))] == n))>0)
                  L.col[i] <- min(which(x[i, intersect(which(x[i,] == max.csum), which(x[i,] == max.rsum))] == n))
                else
                  if(x[i, L.col.max] == max.csum)
                    L.col[i] <- L.col.max
                  else
                    L.col[i] <- min(which(x[i,] == rmax[i]))
              }
              sigma2 <- (n-sum(rmax))*(sum(rmax) + max.csum - 
                                         2*(sum(rmax[which(L.col == L.col.max)])))/(n-max.csum)^3
            }
            , "row" = { 
              L.row.max <- min(which(rsum == max.rsum))
              for(i in 1:nc) {
                if(length(which(x[intersect(which(x[,i] == max.rsum), which(x[,i] == max.csum)),i] == n))>0)
                  L.row[i] <- min(which(x[i,intersect(which(x[i,] == max.csum), which(x[i,] == max.rsum))] == n))
                else
                  if(x[L.row.max,i] == max.rsum)
                    L.row[i] <- L.row.max
                else
                  L.row[i] <- min(which(x[,i] == cmax[i]))
              }
              sigma2 <- (n-sum(cmax))*(sum(cmax) + max.rsum - 
                                         2*(sum(cmax[which(L.row == L.row.max)])))/(n-max.rsum)^3
            }
    )
    
    pr2 <- 1 - (1 - conf.level)/2
    ci <- pmin(1, pmax(0, qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + res))
    res <- c(lambda = res,  lwr.ci=ci[1], ups.ci=ci[2])
  }               
  
  return(res)
}
