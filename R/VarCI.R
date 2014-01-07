VarCI <-
function (x, type = c("classic", "norm","basic","stud","perc","bca"), 
                    conf.level = 0.95, na.rm = FALSE, R=999) {
  
  if (na.rm) x <- na.omit(x)
  type <- match.arg(type, c("classic", "norm","basic","stud","perc","bca"))

  if(type == "classic"){
    df <- length(x) - 1
    v <- var(x)
    res <- c (var = v, lwr.ci = df * v/qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
              , upr.ci = df * v/qchisq((1 - conf.level)/2, df) )

  } else {
    boot.fun <- boot(x, function(x, d) var(x[d], na.rm=na.rm), R=R)
    ci <- boot.ci(boot.fun, conf=conf.level, type=type)
    if(type == "norm"){
      res <- c(mean=boot.fun$t0, lwr.ci=ci[[4]][2], upr.ci=ci[[4]][3])
    } else {
      res <- c(mean=boot.fun$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
    }
  }
  
  return(res)
}
