as.matrix.xtabs <-
function(x, ...){
  
  # xtabs would not be converted by as.matrix.default...
   
  attr(x, "class") <- NULL 
  attr(x, "call") <- NULL 
  
  return(x)
  
}
