CartToSph <-
function (x, y, z, up = TRUE ) { 
  
  vphi <- CartToPol(x, y)          # x, y -> c( w, phi )
  R <- if (up) {
    CartToPol(vphi$r, z)          # ( w, z,  -> r, theta )
  } else {
    CartToPol(z, vphi$r)          # ( z, w,  -> r, theta )
  }
  res <- c(R[1], R[2], vphi[2])
  names(res) <- c("r", "theta", "phi")
  
  return (res)
}
