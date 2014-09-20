SecToHms <-
function(x) {

  l1 <- FALSE
  if (length(x) == 1) {
    x <- c(x, 0)
    l1 <- TRUE
  } 
  h <- floor(x/3600)
  m <- floor((x-h*3600)/60)
  s <- x-(m*60 + h*3600)
  pad <- function(x) sprintf("%02d", as.numeric(x))
  
  out <- apply(apply(cbind(h, m, s), 2, pad), 1, paste, collapse=":")

  if (l1) {
    out <- out[1]
  }
  out
}
