SecToHms <-
function(x, digits=NULL) {
  
  x <- as.numeric(x)
  
  h <- floor(x/3600)
  m <- floor((x-h*3600)/60)
  s <- floor(x-(m*60 + h*3600))
  b <- x-(s + m*60 + h*3600)
  
  if(is.null(digits)) digits <- ifelse(all(b < .Machine$double.eps^0.5),0, 2)
  if(digits==0) f <- "" else f <- gettextf(paste(".%0", digits, "d", sep=""), round(b*10^digits, 0))  

  gettextf("%02d:%02d:%02d%s", h, m, s, f)
  
}
