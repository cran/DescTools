Assocs <-
function(x, conf.level = 0.95){

  # this is from boot::corr combined with ci logic from cor.test
  r <- corr(d=GetPairs(1:nrow(x), 1:ncol(x)), as.vector(x))
  r.ci <- CorCI(rho = r, n = sum(x), conf.level = conf.level)

  res <- rbind(
    "Phi Coeff." = c(Phi(x), NA, NA)
    , "Contingency Coeff." = c(ContCoef(x),NA, NA)
    , "Cramer V" = CramerV(x, conf.level=conf.level)
    , "Goodman Kruskal Gamma" = GoodmanKruskalGamma(x, conf.level=conf.level)
    , "Kendall Tau-b" = KendallTauB(x, conf.level=conf.level)
    , "Stuart Tau-c" = StuartTauC(x, conf.level=conf.level)
    , "Somers D C|R" = SomersDelta(x, direction="column", conf.level=conf.level)
    , "Somers D R|C" = SomersDelta(x, direction="r", conf.level=conf.level)
#    , "Pearson Correlation" =c(cor.p$estimate, lwr.ci=cor.p$conf.int[1], upr.ci=cor.p$conf.int[2])
    , "Pearson Correlation" =c(r.ci[1], lwr.ci=r.ci[2], upr.ci=r.ci[3])
    , "Spearman Correlation" = SpearmanRho(x, conf.level=conf.level)
    , "Lambda C|R" = Lambda(x, direction="column", conf.level=conf.level)
    , "Lambda R|C" = Lambda(x, direction="row", conf.level=conf.level)
    , "Lambda sym" = Lambda(x, direction="sym", conf.level=conf.level)
    , "Uncertainty Coeff. C|R" = UncertCoef(x, direction="column", conf.level=conf.level)
    , "Uncertainty Coeff. R|C" = UncertCoef(x, direction="row", conf.level=conf.level)
    , "Uncertainty Coeff. sym" = UncertCoef(x, direction="sym", conf.level=conf.level)
    , "Mutual Information" = c(MutInf(x),NA,NA)
  )
  
  dimnames(res)[[2]][1] <- "estimate"
  class(res) <- c("Assocs", class(res))
  return(res)
  
}
