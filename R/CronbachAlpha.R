CronbachAlpha <-
function(x, conf.level = NA, cond = FALSE, na.rm = FALSE){
  
  i.CronbachAlpha <- function(x, conf.level = NA){
    nc <- ncol(x)
    colVars <- apply(x, 2, var)
    total   <- var(apply(x, 1, sum))
    res <- (total - sum(colVars)) / total * (nc/(nc-1))
    
    if (!is.na(conf.level)) {
      N <- length(x)
      ci <- 1 - (1-res) * qf( c(1-(1-conf.level)/2, (1-conf.level)/2), N-1, (nc-1)*(N-1))
      res <- c("Cronbach Alpha"=res, lwr.ci=ci[1], upr.ci=ci[2])
    }    
    return(res)
  }
  
  
  x <- as.matrix(x)
  if(na.rm) x <- na.omit(x)
  
  res <- i.CronbachAlpha(x = x, conf.level = conf.level)
  
  if(cond) {  
    condCronbachAlpha <- list()
    n <- ncol(x)
    if(n > 2) {     # can't calculate conditional with only 2 items
      for(i in 1:n){
        condCronbachAlpha[[i]] <- i.CronbachAlpha(x[,-i], conf.level = conf.level)
      }
      condCronbachAlpha <- data.frame(Item = 1:n, do.call("rbind", condCronbachAlpha))
      colnames(condCronbachAlpha)[2] <- "Cronbach Alpha"
    }
    res <- list(unconditional=res, condCronbachAlpha = condCronbachAlpha)
  }
  
  return(res)
}
