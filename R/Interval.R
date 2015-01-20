Interval <-
function(x, y){
  
  # make sure that min is left and max right
  x <- cbind(apply(rbind(x), 1, min), apply(rbind(x), 1, max))
  y <- cbind(apply(rbind(y), 1, min), apply(rbind(y), 1, max))
  
  # replicate
  maxdim <- max(nrow(x), nrow(y))
  x <- x[rep(1:nrow(x), length.out=maxdim), , drop=FALSE]
  y <- y[rep(1:nrow(y), length.out=maxdim), , drop=FALSE]
  
  d <- numeric(maxdim)
  idx <- y[,1] > x[,2]
  d[idx] <- (y[idx,1] - x[idx,2])
  idx <- y[,2] < x[,1]
  d[idx] <- (y[idx,2] - x[idx,1])
  
  unname(d)
}
