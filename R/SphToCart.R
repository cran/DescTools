SphToCart <-
function (r, theta, phi, up = TRUE) {
  
  if (up) theta <- pi/2 - theta  
  
  vz <- PolToCart(r, theta)  
  xy <- PolToCart(vz$y, phi) 
  
  res <- list(x=xy$x, y=xy$x, z=vz$x)
  
  return (res)
}
