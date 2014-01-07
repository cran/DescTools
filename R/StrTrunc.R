StrTrunc <-
function(x, maxlen = 20) {
  paste(substr(x, 0, maxlen), ifelse(nchar(x)>maxlen,"...",""), sep="")
}
