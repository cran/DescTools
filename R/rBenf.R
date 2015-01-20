rBenf <-
function(n, ndigits = 1) {
  if (!IsNumeric(ndigits, length.arg = 1,
                  positive = TRUE, integer.valued = TRUE) ||
        ndigits > 2)
    stop("argument 'ndigits' must be 1 or 2")
  lowerlimit <- ifelse(ndigits == 1, 1, 10)
  upperlimit <- ifelse(ndigits == 1, 9, 99)
  use.n <- if ((length.n <- length(n)) > 1) length.n else
    if (!IsNumeric(n, integer.valued = TRUE,
                    length.arg = 1, positive = TRUE)) 
      stop("bad input for argument 'n'") else n
  myrunif <- runif(use.n)
  
  ans <- rep(lowerlimit, length = use.n)
  for (ii in (lowerlimit+1):upperlimit) {
    indexTF <- (pBenf(ii-1, ndigits = ndigits) < myrunif) &
      (myrunif <= pBenf(ii, ndigits = ndigits))
    ans[indexTF] <- ii
  }
  ans
}
