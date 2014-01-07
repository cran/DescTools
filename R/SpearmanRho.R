SpearmanRho <-
function(x, y = NULL, use = c("everything", "all.obs", "complete.obs", 
            "na.or.complete","pairwise.complete.obs"), conf.level = NA ) { 
  
  if(is.null(y)) {
    x <- Untable(x)
    y <- x[,2]
    x <- x[,1]
  }
  # Reference:
  #   https://stat.ethz.ch/pipermail/r-help/2006-October/114319.html
  # fisher z transformation for calc SpearmanRho ci :
  # Conover WJ, Practical Nonparametric Statistics (3rd edition). Wiley 1999.

  # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
  # pp 1738

 
  # n <- sum(tab)
  # ni. <- apply(tab, 1, sum) 
  # n.j <- apply(tab, 2, sum) 
  # F <- n^3 - sum(ni.^3)
  # G <- n^3 - sum(n.j^3)
  # w <- 1/12*sqrt(F * G)
  
  # ### Asymptotic standard error: sqrt(sigma2)
  # sigma2 <- 1
  # ### debug: print(sqrt(sigma2))
  
  # ### Tau-c = (C - D)*[2m/(n2(m-1))] 
  # est <- 1
  
  # if(is.na(conf.level)){
    # result <- tauc
  # } else {
    # pr2 <- 1 - (1 - conf.level)/2
    # CI <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + est
    # result <- c(SpearmanRho = est,  lwr.ci=max(CI[1], -1), ups.ci=min(CI[2], 1))
  # }               
  
  # return(result)


  # Ref:
  # http://www-01.ibm.com/support/docview.wss?uid=swg21478368
  
  use <- match.arg(use, choices=c("everything", "all.obs", "complete.obs", 
            "na.or.complete","pairwise.complete.obs"))
  
  rho <- cor(as.numeric(x), as.numeric(y), method="spearman", use = use)
  
  e_fx <- exp( 2 * ((.5 * log((1+rho) / (1-rho))) - c(1, -1) * 
      (abs(qnorm((1 - conf.level)/2))) * (1 / sqrt(sum(complete.cases(x,y)) - 3)) ))
  ci <- (e_fx - 1) / (e_fx + 1)
  
  if (is.na(conf.level)) {
    result <- rho
  } else {
    pr2 <- 1 - (1 - conf.level) / 2
    result <- c(rho = rho, lwr.ci = max(ci[1], -1), ups.ci = min(ci[2], 1))
  }
  return(result)
  
}
