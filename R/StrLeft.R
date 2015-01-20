StrLeft <-
function(x, n) {
  n <- rep(n, length.out=length(x))
  sapply(seq_along(x), function(i) substr(x[i], 0, n[i]))
}
