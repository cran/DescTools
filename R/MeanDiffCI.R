MeanDiffCI <-
function (x, y, type = c("classic", "norm","basic","stud","perc","bca"), 
                    conf.level = 0.95, na.rm = FALSE, R=999) {
  
  if (na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }  
  type <- match.arg(type, c("classic", "norm","basic","stud","perc","bca"))
  if(type == "classic"){
      a <- t.test(x, y, conf.level = conf.level)
      res <- c(meandiff = mean(x) - mean(y), lwr.ci = a$conf.int[1], upr.ci = a$conf.int[2])
    
  } else {
    
    diff.means <- function(d, f)
    {    n <- nrow(d)
         gp1 <- 1:table(as.numeric(d[,2]))[1]
         m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
         m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
         m1 - m2
    }
    
    m <- cbind(c(x,y), c(rep(1,length(x)), rep(2,length(y))))
    
    boot.fun <- boot(m, diff.means, R=R, stype="f", strata = m[,2])
    ci <- boot.ci(boot.fun, conf=conf.level, type=type)
    if(type == "norm"){
      res <- c(meandiff=boot.fun$t0, lwr.ci=ci[[4]][2], upr.ci=ci[[4]][3])
    } else {
      res <- c(meandiff=boot.fun$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
    }
  }
  
  return(res)
}
