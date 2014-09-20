IsLeapYear <-
function(x){
  x <- Year(as.Date(x))
  ifelse(x %% 100 == 0, x %% 400 == 0, x %% 4 == 0)
}
