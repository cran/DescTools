Assocs <-
function(x, conf.level = 0.95){

  tab <- x
  
  xy <- Untable(x)
  x <- xy[,1]
  y <- xy[,2]
  
  # cor.test will bark if conf.level is set to NA
  cor.p <- cor.test(as.numeric(x), as.numeric(y), 
                    conf.level=ifelse(is.na(conf.level), 1, conf.level))
  cor.k <- cor.test(as.numeric(x), as.numeric(y), 
                    conf.level=ifelse(is.na(conf.level), 1, conf.level), method="kendall")
  cor.s <- cor.test(as.numeric(x), as.numeric(y), 
                    conf.level=ifelse(is.na(conf.level), 1, conf.level), method="spearman")
  
  res <- rbind(
    "Phi Coeff." = c(Phi(tab), NA, NA)
    , "Contingency Coeff." = c(ContCoef(tab),NA, NA)
    , "Cramer V" = CramerV(tab, conf.level=conf.level)
    , "Goodman Kruskal Gamma" = GoodmanKruskalGamma(tab, conf.level=conf.level)
    , "Kendall Tau-b" = KendallTauB(tab, conf.level=conf.level)
    , "Stuart Tau-c" = StuartTauC(tab, conf.level=conf.level)
    , "Somers D C|R" = SomersDelta(tab, direction="column", conf.level=conf.level)
    , "Somers D R|C" = SomersDelta(tab, direction="r", conf.level=conf.level)
    , "Pearson Correlation" =c(cor.p$estimate, lwr.ci=cor.p$conf.int[1], upr.ci=cor.p$conf.int[2])
    , "Spearman Correlation" = SpearmanRho(tab, conf.level=conf.level)
    , "Lambda C|R" = Lambda(tab, direction="column", conf.level=conf.level)
    , "Lambda R|C" = Lambda(tab, direction="row", conf.level=conf.level)
    , "Lambda sym" = Lambda(tab, direction="sym", conf.level=conf.level)
    , "Uncertainty Coeff. C|R" = UncertCoef(tab, direction="column", conf.level=conf.level)
    , "Uncertainty Coeff. R|C" = UncertCoef(tab, direction="row", conf.level=conf.level)
    , "Uncertainty Coeff. sym" = UncertCoef(tab, direction="sym", conf.level=conf.level)
    , "Mutual Information" = c(MutInf(tab),NA,NA)
  )
  
  dimnames(res)[[2]][1] <- "estimate"
  class(res) <- c("Assocs", class(res))
  return(res)
  
}
