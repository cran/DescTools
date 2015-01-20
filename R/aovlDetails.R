aovlDetails <-
function(aovObj) {
  aovSum  <- summary(aovObj)
  etNames <- names(aovSum)  # error terms
  
  getOneRes <- function(tt, tab) {  # tab=anova table, tt = tested term
    ttIdx <- which(DescTools::StrTrim(rownames(tab)) == tt)
    list(df=tab[ttIdx,       "Df"],
         SS=tab[ttIdx,       "Sum Sq"],
         MS=tab[ttIdx,       "Mean Sq"],
         dfE=tab["Residuals", "Df"],
         SSE=tab["Residuals", "Sum Sq"],
         MSE=tab["Residuals", "Mean Sq"],
         F=tab[ttIdx, "F value"],
         p=tab[ttIdx, "Pr(>F)"])
  }
  
  getTermRes <- function(et) { # et = error term
    tab <- aovSum[[et]][[1]]
    at  <- DescTools::StrTrim(rownames(tab)) # all terms
    tt  <- at[-which(at == "Residuals")]     # tested terms only
    
    if(length(tt) > 0)
    {
      # error terms
      etRes <- list(df=tab["Residuals", "Df"],
                    SS=tab["Residuals", "Sum Sq"],
                    MS=tab["Residuals", "Mean Sq"])
      ttRes <- lapply(tt, getOneRes, tab=tab)
      ttRes <- setNames(ttRes, tt)
      ttIdx <- which(DescTools::StrTrim(rownames(tab)) %in% tt)
      return(data.frame(tt=tt, et=et,
                        tab[ttIdx, , drop=FALSE],
                        dfE=etRes$df, SSE=etRes$SS, MSE=etRes$MS,
                        stringsAsFactors=FALSE))
    } else {
      emptyDf <- data.frame(matrix(ncol=10, nrow=0))
      return(setNames(emptyDf, c("tt", "et", "Df", "Sum.Sq", "Mean.Sq", "F.value",
                                 "Pr..F.", "dfE", "SSE", "MSE")))
    }
  }
  
  detailsL  <- setNames(lapply(etNames, getTermRes), etNames)
  detailsDf <- do.call("rbind", detailsL)
  rownames(detailsDf) <- NULL
  return(detailsDf)
}
