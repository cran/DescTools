RobScale <-
function(x, center = TRUE, scale = TRUE){
  
  x <- as.matrix(x)
  
  if(center) { 
    x <- scale(x, center = apply(x, 2, median, na.rm=TRUE), scale = FALSE)
  }  
  if(scale) {
    x <- scale(x, center = FALSE, scale = apply(x, 2, mad, na.rm=TRUE))
  }  
  return(x)
}
