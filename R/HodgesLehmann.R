HodgesLehmann <-
function(x, y = NULL, conf.level = NA, na.rm = FALSE) {

#   Werner Stahel's version:
#  
#   f.HodgesLehmann <- function(data)
#   {
#     ## Purpose:   Hodges-Lehmann estimate and confidence interval
#     ## -------------------------------------------------------------------------
#     ## Arguments:
#     ## Remark: function changed so that CI covers >= 95%, before it was too
#     ##         small (9/22/04)
#     ## -------------------------------------------------------------------------
#     ## Author: Werner Stahel, Date: 12 Aug 2002, 14:13
#     ## Update: Beat Jaggi, Date: 22 Sept 2004  
#     .cexact <-
#       # c(NA,NA,NA,NA,NA,21,26,33,40,47,56,65,74,84,95,107,119,131,144,158)
#       c(NA,NA,NA,NA,NA,22,27,34,41,48,57,66,75,85,96,108,120,132,145,159)  
#     .d <- na.omit(data)
#     .n <- length(.d)
#     .wa <- sort(c(outer(.d,.d,"+")/2)[outer(1:.n,1:.n,"<=")])
#     .c <- if (.n<=length(.cexact)) .n*(.n+1)/2+1-.cexact[.n] else
#       floor(.n*(.n+1)/4-1.96*sqrt(.n*(.n+1)*(2*.n+1)/24))
#     .r <- c(median(.wa), .wa[c(.c,.n*(.n+1)/2+1-.c)])
#     names(.r) <- c("estimate","lower","upper")
#     .r
#   }

  
  # inspired by package ICSNP, function hl.loc
  
  if(na.rm) {
    if(is.null(y)) 
      x <- na.omit(x)
    else {
      ok <- complete.cases(x, y)
      x <- x[ok]
      y <- y[ok]
    }
  }
  
  if(anyNA(x) || (!is.null(y) && anyNA(y)))
    if(is.na(conf.level)) 
      return(NA)
    else
      return(c(est=NA,  lwr.ci=NA, upr.ci=NA))
  
  
  res <- wilcox.test(x,  y, conf.int = TRUE, conf.level = Coalesce(conf.level, 0.8))
  
  if(is.na(conf.level)){
    result <-  res$estimate
    names(result) <- NULL
  } else {
    result <- c(est=res$estimate,  lwr.ci=res$conf.int[1], upr.ci=res$conf.int[2])
    names(result)[1] <- "est"
  }               
  
  return(result)
  
}
