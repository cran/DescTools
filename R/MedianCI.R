MedianCI <-
function(x, conf.level=0.95, na.rm=FALSE, method=c("exact","boot"), R=999) {
  if(na.rm) x <- na.omit(x)
  
  # alte Version, ziemlich grosse Unterschiede zu wilcox.test:
  # Bosch: Formelsammlung Statistik (bei Markus Naepflin), S. 95 
  # x <- sort(x)
  # return( c( 
  # x[ qbinom(alpha/2,length(x),0.5) ], ### lower limit
  # x[ qbinom(1-alpha/2,length(x),0.5) ] ### upper limit
  # ) )
    
  switch( match.arg(arg=method, choices=c("exact","boot"))
          , "exact" = { # this is the SAS-way to do it
            # https://stat.ethz.ch/pipermail/r-help/2003-September/039636.html
            r <- SignTest(x)$conf.int
          }
          , "boot" = {
              boot.med <- boot(x, function(x, d) median(x[d], na.rm=na.rm), R=R)
              r <- boot.ci(boot.med, conf=conf.level, type="basic")[[4]][4:5]
          } )
  
  med <- median(x, na.rm=na.rm)
  if(is.na(med)) {   # do not report a CI if the median is not defined...
    r <- rep(NA, 3)
  } else {
    r <- c(median=med, r)
  }  
  names(r) <- c("median","lwr.ci","upr.ci")
  return( r )
  
}
