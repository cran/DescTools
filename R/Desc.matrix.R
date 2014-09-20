Desc.matrix <-
function(x, main=NULL, rfrq = NULL, margins = c(1,2), plotit=getOption("plotit", FALSE), verbose = c("medium","low","high"), ... ){
  Desc.table(x, main=main, rfrq = rfrq, margins = margins, plotit = plotit, verbose = verbose, ... )
  invisible()
}
