StrChop <-
function(x, len) {
  # example: StrChop(x=paste(letters, collapse=""), len = c(3,5,0))
  xsplit <- character(0)
  for(i in 1:length(len)){
    xsplit <- append(xsplit, substr(x, 1, len[i]))
    x <- substr(x, len[i]+1, nchar(x)-len[i])
  }
  return(xsplit)
}
