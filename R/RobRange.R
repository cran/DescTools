RobRange <-
function(x, trim = 0.2, fac = 3, na.rm = FALSE) {

  # author: Werner Stahel 
  # from:   regr.r
  
  if(na.rm) x <- na.omit(x)
  
  ldat <- x[is.finite(x)]
  if (is.character(ldat)|length(ldat) == 0) stop("invalid data")
  trim <- c(trim, 0.2)[1]
  if (!is.finite(trim)) trim <- 0.2
  lmn <- mean(ldat, trim=trim)
  lds <- sort(abs(ldat - lmn))
  ln <- ceiling((1 - trim) * length(ldat))
  if (ln < 3) {
    warning("Not enough valid data. returning ordinary range")
    lsd <- Inf 
  } else {
    lsd <- fac * sum(lds[1:ln] / (ln-1))
    if (lsd == 0) {
      warning("Robust range has width 0. returning ordinary range")
      lsd <- Inf }
  }
  c(max(lmn - lsd, min(ldat)), min(lmn + lsd, max(ldat)))
  
}
