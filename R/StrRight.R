StrRight <-
function(x, n) {
  n <- rep(n, length.out=length(x))
  sapply(seq_along(x), function(i) substr(x[i], (nchar(x[i]) - n[i]+1), nchar(x[i])))
}
