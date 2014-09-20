BreslowDayTest <-
function(x, OR = NA, correct = FALSE) {
  
  # Function to perform the Breslow and Day (1980) test including
  # the corrected test by Tarone
  # Uses the equations in Lachin (2000) p. 124-125.
  #
  # Programmed by Michael Hoehle <http://www-m4.ma.tum.de/pers/hoehle>
  # Note that the results of the Tarone corrected test do
  # not correspond to the numbers in the Lachin book...
  #
  # Params:
  #  x - a 2x2xK contingency table
  #  correct - if TRUE Tarones correction is returned
  #
  # Returns:
  #  a vector with three values
  #   statistic - Breslow and Day test statistic
  #   pval - p value evtl. based on the Tarone test statistic
  #               using a \chi^2(K-1) distribution
  #
  
  
  if(is.na(OR)) {
    #Find the common OR based on Mantel-Haenszel
    or.hat.mh <- mantelhaen.test(x)$estimate
  } else {
    or.hat.mh <- OR
  }
  
  #Number of strata
  K <- dim(x)[3]
  #Value of the Statistic
  X2.HBD <- 0
  #Value of aj, tildeaj and Var.aj
  a <- tildea <- Var.a <- numeric(K)
  
  for (j in 1:K) {
    #Find marginals of table j
    mj <- apply(x[,,j], MARGIN=1, sum)
    nj <- apply(x[,,j], MARGIN=2, sum)
    
    #Solve for tilde(a)_j
    coef <- c(-mj[1]*nj[1] * or.hat.mh, nj[2]-mj[1]+or.hat.mh*(nj[1]+mj[1]),
              1-or.hat.mh)
    sols <- Re(polyroot(coef))
    #Take the root, which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
    tildeaj <- sols[(0 < sols) &  (sols <= min(nj[1],mj[1]))]
    #Observed value
    aj <- x[1,1,j]
    
    #Determine other expected cell entries
    tildebj <- mj[1] - tildeaj
    tildecj <- nj[1] - tildeaj
    tildedj <- mj[2] - tildecj
    
    #Compute \hat{\Var}(a_j | \widehat{\OR}_MH)
    Var.aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)
    
    #Compute contribution
    X2.HBD <- X2.HBD + as.numeric((aj - tildeaj)^2 / Var.aj)
    
    #Assign found value for later computations
    a[j] <- aj ;  tildea[j] <- tildeaj ; Var.a[j] <- Var.aj
  }
  
  # Compute Tarone corrected test
  X2.HBDT <-as.numeric( X2.HBD -  (sum(a) - sum(tildea))^2/sum(Var.aj) )
  
  DNAME <- deparse(substitute(x))
  
  STATISTIC <- if(correct) X2.HBDT else X2.HBD
  PARAMETER <- K - 1
  # Compute p-value based on the Tarone corrected test
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
  METHOD <- if(correct) "Breslow-Day Test for Homogeneity of the Odds Ratios (with Tarone correction)" else 
    "Breslow-Day Test for Homogeneity of the Odds Ratios"
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = STATISTIC, parameter = PARAMETER, 
                 p.value = PVAL, method = METHOD, data.name = DNAME 
  ), class = "htest")
  
}
