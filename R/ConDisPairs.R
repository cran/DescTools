ConDisPairs <-
function(x){
  
  # tab is a matrix of counts   
  # Based on code of Michael Friendly and Laura Thompson

  # slooooow because of 2 nested for clauses O(n^2)
  # this is NOT faster when implemented with a mapply(...)

  # Lookin for alternatives in C
  # http://en.verysource.com/code/1169955_1/kendl2.cpp.html  
  # cor(..., "kendall") is for dimensions better
  
  n <- nrow(x)
  m <- ncol(x)
  pi.c <- pi.d <- matrix(0, nrow = n, ncol = m)
  
  row.x <- row(x)
  col.x <- col(x)
  
  for(i in 1:n){
    for(j in 1:m){
      pi.c[i, j] <- sum(x[row.x<i & col.x<j]) + sum(x[row.x>i & col.x>j])
      pi.d[i, j] <- sum(x[row.x<i & col.x>j]) + sum(x[row.x>i & col.x<j])
    }
  }
  C <- sum(pi.c * x)/2
  D <- sum(pi.d * x)/2
  
  return(list(pi.c = pi.c, pi.d = pi.d, C = C, D = D))
  
}
