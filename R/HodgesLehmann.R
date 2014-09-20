HodgesLehmann <-
function(x, y = NULL, conf.level = NA, na.rm = FALSE) {
  
  # inspired by package ICSNP, function hl.loc
  if (na.rm) x <- na.omit(x)
  
  res <- wilcox.test(x,  y, conf.int = TRUE, conf.level = Coalesce(conf.level, 0.8))
  
  if(is.na(conf.level)){
    result <-  res$estimate
  } else {
    result <- c(est = res$estimate,  lwr.ci=res$conf.int[1], upr.ci=res$conf.int[2])
  }               
  
  names(result)[1] <- "est"
  return(result)
  
}
