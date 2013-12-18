StrCountW <-
function(x){
  # old:    does not work for one single word!!
  # return(sapply(gregexpr("\\b\\W+\\b", x, perl=TRUE), length) + 1)
  return(sapply(gregexpr("\\b\\W+\\b", x, perl = TRUE), function(x) sum(x>0)) + 1)
}
