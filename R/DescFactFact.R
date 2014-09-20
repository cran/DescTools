DescFactFact <-
function( x, grp, rfrq="100" # , show.mutinf=FALSE
  , xname=deparse(substitute(x)), grpname=deparse(substitute(grp)), plotit=getOption("plotit", FALSE)) {
  tab <- table(x, grp) 
  Desc(tab, rfrq=rfrq, xname=xname, plotit=FALSE)
  main <- gettextf("%s ~ %s", xname, grpname)
  if(plotit) PlotDesc.table(tab, main=main)
  
  invisible()
  
}
