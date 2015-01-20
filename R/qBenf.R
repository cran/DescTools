qBenf <-
function(p, ndigits = 1) {
  if (!IsNumeric(ndigits, length.arg = 1,
                  positive = TRUE, integer.valued = TRUE) ||
        ndigits > 2)
    stop("argument 'ndigits' must be 1 or 2")
  lowerlimit <- ifelse(ndigits == 1, 1, 10)
  upperlimit <- ifelse(ndigits == 1, 9, 99)
  bad <- !is.na(p) & !is.nan(p) & ((p < 0) | (p > 1))
  if (any(bad))
    stop("bad input for argument 'p'")
  
  ans <- rep(lowerlimit, length = length(p))
  for (ii in (lowerlimit+1):upperlimit) {
    indexTF <- is.finite(p) &
      (pBenf(ii-1, ndigits = ndigits) < p) &
      (p <= pBenf(ii, ndigits = ndigits))
    ans[indexTF] <- ii
  }
  
  ans[ is.na(p) |  is.nan(p)] <- NA
  ans[!is.na(p) & !is.nan(p) & (p == 0)] <- lowerlimit
  ans[!is.na(p) & !is.nan(p) & (p == 1)] <- upperlimit
  ans
}
