Coalesce <-
function(..., method = c("is.na", "is.finite")) {
  # Returns the first element in x which is not NA
  
  if(length(list(...)) > 1) { 
    if(all(lapply(list(...), length) > 1)){
      x <- data.frame(...)
    } else {
      x <- unlist(list(...))
    }  
  } else {
    if(is.matrix(...)) {
      x <- data.frame(...)
    } else {  
      x <- (...)
    }  
  }
  switch(match.arg(method, choices=c("is.na", "is.finite")),
    "is.na" = res <- Reduce(function (x,y) ifelse(!is.na(x), x, y), x), 
    "is.finite" = res <- Reduce(function (x,y) ifelse(is.finite(x), x, y), x) 
  )  
  return(res)
}
