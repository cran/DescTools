CartToPol <-
function(x, y) { 
  theta <- atan(y/x)
  theta[x<0] <- theta[x<0] + pi    # atan can't find the correct square (quadrant)
  list( r = sqrt(x^2+y^2), theta=theta )
}
