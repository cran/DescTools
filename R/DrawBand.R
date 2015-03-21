DrawBand <-
function(x, y, col = SetAlpha("grey", 0.5), border = NA) {

  # accept matrices but then only n x y
  if(!identical(dim(y), dim(x))){
    x <- as.matrix(x)
    y <- as.matrix(y)

    if(dim(x)[2] == 1 && dim(y)[2] == 2)
      x <- x[, c(1,1)]
    else if(dim(x)[2] == 2 && dim(y)[2] == 1)
      y <- y[, c(1,1)]
    else 
      stop("incompatible dimensions for matrices x and y")
    
    x <- c(x[,1], rev(x[,2]))
    y <- c(y[,1], rev(y[,2]))
    
  }  
  
  # adds a band to a plot, normally used for plotting confidence bands
  polygon(x=x, y=y, col = col, border = border)
}
