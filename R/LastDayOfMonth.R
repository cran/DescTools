LastDayOfMonth <-
function(x){
  z <- AddMonths(x, 1)
  Day(z) <- 1
  return(z-1)
}
