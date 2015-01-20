PlotDesc.ordered <-
function(x, ..., wrd=NULL) { 
  mf <- match.call(expand.dots = FALSE) 
  mf$...["ord"] <- InDots(..., arg="ord", default = "level")
  # error if main is not treated here... why??
  mf$...["main"] <- InDots(..., arg="main", default = deparse(substitute(x)))
  args <- append(list(x=x, wrd=wrd), mf$...)
  do.call(PlotDesc.factor, args)
  
}
