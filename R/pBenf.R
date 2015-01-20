pBenf <-
function(q, ndigits = 1, log.p = FALSE) {
  if (!IsNumeric(ndigits, length.arg = 1,
                  positive = TRUE, integer.valued = TRUE) ||
        ndigits > 2)
    stop("argument 'ndigits' must be 1 or 2")
  lowerlimit <- ifelse(ndigits == 1, 1, 10)
  upperlimit <- ifelse(ndigits == 1, 9, 99)
  
  ans <- q * NA
  floorq <- floor(q)
  indexTF <- is.finite(q) & (floorq >= lowerlimit)
  ans[indexTF] <- log10(1 + floorq[indexTF]) -
    ifelse(ndigits == 1, 0, 1)
  ans[!is.na(q) & !is.nan(q) & (q >= upperlimit)] <- 1
  ans[!is.na(q) & !is.nan(q) & (q <  lowerlimit)] <- 0
  if (log.p) log(ans) else ans
}
