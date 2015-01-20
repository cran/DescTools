Outlier <-
function(x, method=c("boxplot"), na.rm=FALSE){
  
  if(na.rm) x <- na.omit(x)
  
  switch(match.arg(arg = method, choices = c("boxplot")), 
  #         boxplot =  { x[x %)(% (quantile(x, c(0.25,0.75), na.rm=na.rm) + c(-1,1) * 1.5*IQR(x,na.rm=na.rm))] }
     boxplot =  { res <- boxplot(x, plot = FALSE)$out }
  
  )
  return(res)
  
}
