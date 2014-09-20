NPV <-
function(i, cf, t=seq(along=cf)-1) {
  # Net present value
  sum(cf/(1+i)^t)
}
