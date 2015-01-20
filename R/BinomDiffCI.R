BinomDiffCI <-
function(x1, n1, x2, n2, conf.level = 0.95, 
                        method=c("wald", "waldcor", "ac","exact","newcombe","newcombecor","fm","ha")) {
  #   .Wald #1 
  #   .Wald (Corrected) #2 
  #   .Exact 
  #   .Exact (FM Score) 
  #   .Newcombe Score #10 
  #   .Newcombe Score (Corrected) #11 
  #   .Farrington-Manning 
  #   .Hauck-Anderson
  # http://www.jiangtanghu.com/blog/2012/09/23/statistical-notes-5-confidence-intervals-for-difference-between-independent-binomial-proportions-using-sas/
  #  Interval estimation for the difference between independent proportions: comparison of eleven methods.
  
  method <- match.arg(arg = method, 
                      choices = c("wald", "waldcor", "ac","exact","newcombe","newcombecor","fm","ha"))
  
  alpha <- 1 - conf.level
  kappa <- qnorm(1 - alpha/2)
  
  p1.hat <- x1/n1 
  p2.hat <- x2/n2 
  est <- p2.hat - p1.hat
  
  switch(method,
         "wald" = {  vd <- p1.hat*(1-p1.hat)/n1 + p2.hat*(1-p2.hat)/n2
                     term2 <- sign(est) * kappa * sqrt(vd)
                     
                     CI.lower <- max(-1, est - term2)
                     CI.upper <- min(1, est + term2)
         },
         
         "waldcor" = {
           vd <- p1.hat*(1-p1.hat)/n1 + p2.hat*(1-p2.hat)/n2
           
           term2 <- sign(est) * kappa * sqrt(vd)
           term2 <- term2 + 0.5 * (1/n1+1/n2)
           
           CI.lower <- max(-1, est - term2)
           CI.upper <- min(1, est + term2)
         },
         "ac" = {   # Agresti-Caffo
           
           n1 <- n1+2
           n2 <- n2+2
           x1  <- x1+1
           x2  <- x2+1
           
           p1.hat <- x1/n1 
           p2.hat <- x2/n2 
           est1 <-  p2.hat - p1.hat
           
           vd <- p1.hat*(1-p1.hat)/n1 + p2.hat*(1-p2.hat)/n2
           
           term2 <- sign(est1) * kappa * sqrt(vd)
           
           CI.lower <- max(-1, est1 - term2)
           CI.upper <- min(1, est1 + term2)
         } , 
         "newcombe" = {   # Newcombe
           
           w1 <- BinomCI(x=x1, n=n1, conf.level=conf.level, method="wilson")
           w2 <- BinomCI(x=x2, n=n2, conf.level=conf.level, method="wilson")
           l1 <- w1[2]
           u1 <- w1[3]
           l2 <- w2[2]
           u2 <- w2[3]
           
           CI.lower <- max(-1, est + kappa * sqrt(l1*(1-l1)/n1 + u2*(1-u2)/n2))
           CI.upper <- min( 1, est - kappa * sqrt(u1*(1-u1)/n1 + l2*(1-l2)/n2))
         }
  )
  
  ci <- c(est = est, lwr.ci = min(CI.lower, CI.upper), upr.ci = max(CI.lower, CI.upper))
  return(ci)
  
}
