KendallTauB <-
function(x, y = NULL, conf.level = NA, ...){

  # Ref: http://www.fs.fed.us/psw/publications/lewis/LewisHMP.pdf
  # pp 2-9
  # 
  if (!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  } else {
    dname <- deparse(substitute(x))
  }

  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)

  x <- ConDisPairs(tab)

  n <- sum(tab)
  n0 <- n*(n-1)/2
  ti <- apply(tab, 1, sum)
  uj <- apply(tab, 2, sum)
  n1 <- sum(ti * (ti-1) / 2)
  n2 <- sum(uj * (uj-1) / 2)
  
  taub <- (x$C - x$D) / sqrt((n0-n1)*(n0-n2))
  
  pi <- tab / sum(tab)

  pdiff <- (x$pi.c - x$pi.d) / sum(tab)
  Pdiff <- 2 * (x$C - x$D) / sum(tab)^2 
  
  rowsum <- apply(pi, 1, sum)
  colsum <- apply(pi, 2, sum)

  rowmat <- matrix(rep(rowsum, dim(tab)[2]), ncol = dim(tab)[2])
  colmat <- matrix(rep(colsum, dim(tab)[1]), nrow = dim(tab)[1], byrow = T)

  delta1 <- sqrt(1 - sum(rowsum^2))
  delta2 <- sqrt(1 - sum(colsum^2))
  
  # Compute asymptotic standard errors taub
  tauphi <- (2 * pdiff + Pdiff * colmat) * delta2 * delta1 + (Pdiff * rowmat * delta2)/delta1 
  sigma2 <- ((sum(pi * tauphi^2) - sum(pi * tauphi)^2)/(delta1 * delta2)^4) / n

  if (is.na(conf.level)) {
    result <- taub
  }
  else {
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + taub
    result <- c(tau_b = taub, lwr.ci = max(ci[1], -1), ups.ci = min(ci[2], 1))
  }
  
#   if(test){
# 
#     alternative <- match.arg(alternative)
#     
#     zstat <- taub / sqrt(sigma2)
#     
#     if (alternative == "less") {
#       pval <- pnorm(zstat)
#       cint <- c(-Inf, zstat + qnorm(conf.level))
#     }
#     else if (alternative == "greater") {
#       pval <- pnorm(zstat, lower.tail = FALSE)
#       cint <- c(zstat - qnorm(conf.level), Inf)
#     }
#     else {
#       pval <- 2 * pnorm(-abs(zstat))
#       alpha <- 1 - conf.level
#       cint <- qnorm(1 - alpha/2)
#       cint <- zstat + c(-cint, cint)
#     }
#     
#     RVAL <- list()
#     RVAL$p.value <- pval
#     RVAL$method <- "Kendall's rank correlation tau"
#     RVAL$data.name <- dname
#     RVAL$statistic <- x$C - x$D
#     names(RVAL$statistic) <- "T"
#     RVAL$estimate <- taub
#     names(RVAL$estimate) <- "tau-b"
#     RVAL$conf.int <- c(max(ci[1], -1), min(ci[2], 1))
#   #  attr(RVAL$conf.int, "conf.level") = round(attr(ci,"conf.level"), 3)
#     class(RVAL) <- "htest"
#     return(RVAL)
# 
# #     rval <- list(statistic = zstat, p.value = pval, 
# #                  parameter = sd_pop,
# #                  conf.int = cint, estimate = estimate, null.value = mu, 
# #                  alternative = alternative, method = method, data.name = dname)
# 
#   } else {
    return(result)
#   }
 
}
