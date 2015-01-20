aovlErrorTerms <-
function(aovObj) {
  aovSum  <- summary(aovObj)
  etNames <- names(aovSum)
  getSS <- function(x) {
    aovSum[[x]][[1]]["Residuals", "Sum Sq"]
  }
  
  getMS <- function(x) {
    aovSum[[x]][[1]]["Residuals", "Mean Sq"]
  }
  
  getDF <- function(x) {
    aovSum[[x]][[1]]["Residuals", "Df"]
  }
  
  SS <- vapply(etNames, getSS, numeric(1))
  MS <- vapply(etNames, getMS, numeric(1))
  DF <- vapply(etNames, getDF, numeric(1))
  return(list(SS=SS, MS=MS, DF=DF))
}
