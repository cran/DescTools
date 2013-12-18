Assocs <-
function(x, conf.level = 0.95, digits = 4){

  tab <- x
  
  xy <- Untable(x)
  x <- xy[,1]
  y <- xy[,2]
  
  cor.p <- cor.test(as.numeric(x), as.numeric(y), conf.level=conf.level)
  cor.k <- cor.test(as.numeric(x), as.numeric(y), conf.level=conf.level, method="kendall")
  cor.s <- cor.test(as.numeric(x), as.numeric(y), conf.level=conf.level, method="spearman")
  
  res <- rbind(
    "Phi Coeff." = round(c(Phi(tab), NA,NA), digits)
    , "Contingency Coeff." = round(c(ContCoef(tab),NA,NA), digits)
    , "Cramer V" = round(CramerV(tab, conf.level=conf.level), digits)
    , "Goodman Kruskal Gamma" = round(GoodmanKruskalGamma(tab, conf.level=conf.level), digits)
    , "Kendall Tau-b" = round(KendallTauB(tab, conf.level=conf.level), digits)
    , "Stuart Tau-c" = round(StuartTauC(tab, conf.level=conf.level), digits)
    , "Somers D C|R" = round(SomersDelta(tab, direction="column", conf.level=conf.level), digits)
    , "Somers D R|C" = round(SomersDelta(tab, direction="r", conf.level=conf.level), digits)
    , "Pearson Correlation" =round(c(cor.p$estimate, lwr.ci=cor.p$conf.int[1], upr.ci=cor.p$conf.int[2]), digits)
    , "Spearman Correlation" = round(SpearmanRho(tab, conf.level=conf.level), digits)
    , "Lambda C|R" = round(Lambda(tab, direction="column", conf.level=conf.level), digits)
    , "Lambda R|C" = round(Lambda(tab, direction="row", conf.level=conf.level), digits)
    , "Lambda sym" = round(Lambda(tab, direction="sym", conf.level=conf.level), digits)
    , "Uncertainty Coeff. C|R" = round(UncertCoef(tab, direction="column", conf.level=conf.level), digits)
    , "Uncertainty Coeff. R|C" = round(UncertCoef(tab, direction="row", conf.level=conf.level), digits)
    , "Uncertainty Coeff. sym" = round(UncertCoef(tab, direction="sym", conf.level=conf.level), digits)
    , "Mutual Information" = round(c(MutInf(tab),NA,NA), digits)
  )
  
  dimnames(res)[[2]][1] <- "estimate"

  return(res)
  
}
