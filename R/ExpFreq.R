ExpFreq <-
function(x, freq = c("abs", "rel")) {

  # returns the expected frequencies of a table assuming independence

  # this is a copy of independence_table {vcd}
  # by David Meyer David.Meyer@R-project.org
  
  if (!is.array(x)) 
    stop("Need array of absolute frequencies!")
  frequency <- match.arg(freq)
  n <- sum(x)
  x <- x/n
  d <- dim(x)
  margins <- lapply(1:length(d), function(i) apply(x, i, sum))
  tab <- array(apply(expand.grid(margins), 1, prod), d, dimnames = dimnames(x))
  if (frequency == "rel") 
    tab
  else tab * n
}
