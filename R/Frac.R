Frac <-
function(x, dpwr = NA) {  # fractional part
  res <- abs(x) %% 1
  # Alternative: res <- abs(x-trunc(x))
  if (!missing(dpwr)) res <- round(10^dpwr * res)
  res
}
