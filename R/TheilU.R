TheilU <-
function(a, p, type = c(2, 1), na.rm = FALSE){
  
  if(na.rm) {
    idx <- complete.cases(a, p)
    a <- a[idx]
    p <- p[idx]
  }  
  n <- length(a)
  if(length(p)!=n) {
    warning("a must have same length as p")
    res <- NA
  } else { 
    switch( match.arg(as.character(type), c("2", "1"))
            , "1" = { res <- sqrt(sum((a-p)^2/n))/(sqrt(sum(a^2)/n) + sqrt(sum(p^2)/n)) }
            , "2" = { res <- sqrt(sum((a-p)^2))/(sqrt(sum(a^2))) }
            )
  }
  return(res)
  
}
