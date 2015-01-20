DescFactNum <-
function(x, y, xname=deparse(substitute(x))
                       , yname=deparse(substitute(y)), plotit=getOption("plotit", FALSE), digits = NULL, ...) {
  
  # use the other way round first
  DescNumFact(x = y, grp = x, plotit=FALSE, digits=digits, ... )
  
  xy <- na.omit(data.frame(x=x, y=y))
  
  ptab <- prop.table(table(unname(xy$x), CutQ(xy$y, probs = seq(0,1,0.1), na.rm = TRUE)), 2)
  cat(gettextf("\nProportions of %s in the quantiles of %s:\n", xname , yname))
  print(round(ptab,3), quote=FALSE)
  cat("\n")
  
  if(plotit) PlotDescFactNum(y, x, ptab, main=paste(xname, " ~ ", yname, sep=""))
  invisible()
  
}
