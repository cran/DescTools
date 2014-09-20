IRR <-
function(cf, t=seq(along=cf)-1) { 
  # internal rate of return
  uniroot(NPV, c(0,1), cf=cf, t=t)$root 
}
