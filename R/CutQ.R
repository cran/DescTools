CutQ <-
function(x, quant=c(.25,.5,.75,1), labels=gettextf("Q%s", 1:4), na.rm = FALSE){
  cut(x, breaks=quantile(x, breaks=quant, na.rm = na.rm), include.lowest=TRUE, labels=labels)
}
