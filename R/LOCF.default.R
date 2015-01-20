LOCF.default <-
function(x) {

  # last observation carried forward
  # replaces NAs by the last observed value
  
#   while(any(is.na(x))) {
#     x[is.na(x)] <- x[which(is.na(x))-1]
#   }
#   return(x)

  # faster solution from Daniel Wollschlaeger:
  rep(x[!is.na(x)], diff(c(which(!is.na(x)), length(x)+1)))
  
}
