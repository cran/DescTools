MeanCI <-
function (x, trim = 0, type = c("classic", "norm","basic","stud","perc","bca"), 
                    conf.level = 0.95, na.rm = FALSE, R=999) {
  
  if (na.rm) x <- na.omit(x)
  type <- match.arg(type, c("classic", "norm","basic","stud","perc","bca"))
  if(type == "classic"){
    if(trim != 0) {
      # we cannot return classic intervals for the trimmed mean
      res <- NA
    } else {
      a <- qt(p = (1 - conf.level)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
      res <- c(mean = mean(x), lwr.ci = mean(x) + a, upr.ci = mean(x) - a)
    }
    
  } else {
    boot.fun <- boot(x, function(x, d) mean(x[d], na.rm=na.rm, trim=trim), R=R)
    ci <- boot.ci(boot.fun, conf=conf.level, type=type)
    if(type == "norm"){
      res <- c(mean=boot.fun$t0, lwr.ci=ci[[4]][2], upr.ci=ci[[4]][3])
    } else {
      res <- c(mean=boot.fun$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
    }
  }
  
  return(res)
}
