TukeyBiweight <-
function(x, const=9, na.rm = FALSE, conf.level = NA, ci.type = "bca", R=1000, ...) {

  if(na.rm) x <- na.omit(x)
  if(anyNA(x)) return(NA)
  
  if(is.na(conf.level)){
    #  .Call("tbrm", as.double(x[!is.na(x)]), const)
    res <- .Call("tbrm", as.double(x), const)
    
  } else {
    
    
    # adjusted bootstrap percentile (BCa) interval  
    boot.tbw <- boot(x, function(x, d) .Call("tbrm", as.double(x[d]), const), R=R, ...)
    ci <- boot.ci(boot.tbw, conf=conf.level, type=ci.type)
    res <- c(kurt=boot.tbw$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
  }
  
  return(res)
  
}
