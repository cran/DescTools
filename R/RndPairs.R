RndPairs <-
function(n, r, rdist1 = rnorm(n=n, mean = 0, sd = 1), rdist2 = rnorm(n=n, mean = 0, sd = 1)){

  # create correlated random pairs
  data.frame(matrix(nrow=n, ncol=2, data=cbind(rdist1, rdist2)) %*%
                chol(matrix(nrow=2, ncol=2, data=c(1, r, r, 1)))) 
}
