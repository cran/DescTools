IsWhole <-
function(x, tol = .Machine$double.eps^0.5, na.rm=FALSE) {
  # Define check if integer as (source: help-file from is.integer, example section)

  # example:  
  # x <- c(1,3,2.003)
  # all(IsWhole(x))

  # an alternative in cwhmisc :
  #   whole.number <- function (x) all((x%%1) == 0)
  
  if(na.rm) x <- na.omit(x)  
  abs(x - round(x)) < tol
}
