TukeyBiweight <-
function(x, const=9, na.rm = FALSE) {
  if(na.rm) x <- na.omit(x)
  .Call("tbrm", as.double(x[!is.na(x)]), const)
}
