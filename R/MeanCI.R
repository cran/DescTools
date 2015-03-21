MeanCI <-
function (x, sd = NULL, trim = 0, method = c("classic", "boot"), 
                    conf.level = 0.95, na.rm = FALSE, ...) {
  
  if (na.rm) x <- na.omit(x)

  winvar <- function(x, trim) {
    n <- length(x)
    # calculate the winsorized variance of x
    trn <- floor(trim * n) + 1
    
# new 17.2.2015:
    minval <- sort(x, partial = trn)[trn]
    maxval <- sort(x, partial = max((n - trn + 1), 1))[max((n - trn + 1), 1)]
    winvar <- var(Winsorize(x, minval = minval, maxval = maxval))
    
# This was an overkill, we need only the n-thest value here:
# winvar <- var(Winsorize(x, minval=max(Small(x, trn)), maxval=min(Large(x, trn))))
# 
    # degrees of freedom
    DF <- n - 2*(trn-1) - 1
    return(c(var=winvar, DF=DF))
  }
  
  method <- match.arg(method, c("classic", "boot"))
  if(method == "classic"){
    if(trim != 0) {
      # see: http://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v27.txt
      #      http://www.psychology.mcmaster.ca/bennett/boot09/rt2.pdf
      
      wvar <- winvar(x, trim)
      # the standard error
      se <- sqrt(wvar["var"]) / ((1 - 2*trim) * sqrt(length(x)))
      
      res <- mean(x, trim = trim) + c(0, -1, 1) * qt(1-(1-conf.level)/2, wvar["DF"]) * se
      names(res) <- c("mean", "lwr.ci", "upr.ci")
      
    } else {
      if(is.null(sd)) {
        a <- qt(p = (1 - conf.level)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
      } else {
        a <- qnorm(p = (1 - conf.level)/2) * sd/sqrt(length(x))
      }
      res <- c(mean = mean(x), lwr.ci = mean(x) + a, upr.ci = mean(x) - a)
    }
    
  } else {
    
    # see: http://www.psychology.mcmaster.ca/bennett/boot09/percentileT.pdf
    # this might contain an erroneuous calculation of boot variance...
    
    btype <- InDots(..., arg="type", default="basic")
    
    # we need separate functions for trimmed means and normal means
    if(trim != 0) {
      boot.fun <- boot(x, 
                       function(x, i){
                         # this is according to the example in boot.ci
                         m <- mean(x[i], na.rm = FALSE, trim = trim)
                         n <- length(i)
                         v <- winvar(x, trim)/((1-2*trim)*sqrt(length(x)))^2
                         c(m, v)
                       }, 
                       R=InDots(..., arg="R", default=999), 
                       parallel=InDots(..., arg="parallel", default="no"))
      
    } else {
      boot.fun <- boot(x, 
                       function(x, i){
                         # this is according to the example in boot.ci
                         m <- mean(x[i], na.rm = FALSE)
                         n <- length(i)
                         v <- (n-1) * var(x[i]) / n^2
                         # v <- (sd(x[i]) / sqrt(n))^2  # following Bennet 
                         c(m, v)
                         # IMPORTANT: boot.ci requires the estimated VARIANCE of the statistic
                         # pop sd estimated from bootstrapped sample
                       }, 
                       R=InDots(..., arg="R", default=999), 
                       parallel=InDots(..., arg="parallel", default="no"))
    }
    ci <- boot.ci(boot.fun, conf=conf.level, type=btype)

    if(btype == "norm"){
      res <- c(mean=boot.fun$t0[1], lwr.ci=ci[[4]][2], upr.ci=ci[[4]][3])
    } else {
      res <- c(mean=boot.fun$t0[1], lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
    }
  }
  
  return(res)
}
