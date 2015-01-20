ZeroIfNA <-
function(x) { 
#  same as zeroifnull in SQL
  replace(x, is.na(x), 0) 
}
