dBenf <-
function(x, ndigits = 1, log = FALSE) {
  if (!IsNumeric(ndigits, length.arg = 1,
                  positive = TRUE, integer.valued = TRUE) ||
        ndigits > 2)
    stop("argument 'ndigits' must be 1 or 2")
  lowerlimit <- ifelse(ndigits == 1, 1, 10)
  upperlimit <- ifelse(ndigits == 1, 9, 99)
  
  if (!is.logical(log.arg <- log) || length(log) != 1)
    stop("bad input for argument 'log'")
  rm(log)
  
  
  ans <- x * NA
  indexTF <- is.finite(x) & (x >= lowerlimit)
  
  ans[indexTF] <- log10(1 + 1/x[indexTF])
  ans[!is.na(x) & !is.nan(x) &
        ((x < lowerlimit) |
           (x > upperlimit) |
           (x != round(x)))] <- 0.0
  if (log.arg) log(ans) else ans
}
