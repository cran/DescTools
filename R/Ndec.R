Ndec <-
function(x) {
  # liefert die Anzahl der Nachkommastellen einer Zahl x
  # Alternative auch format.info [1]... Breite, [2]...Anzahl Nachkommastellen, [3]...Exponential ja/nein
  stopifnot(class(x)=="character")
  
  res <- rep(0, length(x))
  # remove evtl. exponents
  x <- gsub(pattern="[eE].+$", replacement="", x=x)
  res[grep("\\.",x)] <- nchar( sub("^.+[.]","",x) )[grep("\\.",x)]

  return(res)
  
}
