SampleTwins <-
function (x, stratanames = NULL, twins, 
                         method = c("srswor", "srswr", "poisson", "systematic"), 
                         pik, description = FALSE) {
  
  # sort data first
  x <- x[do.call("order", lapply(x[,stratanames], order)),]
  
  # define the frequencies
  twinsize <- as.data.frame.table(xtabs( as.formula(gettextf("~ %s", paste(stratanames, collapse="+"))), twins))
  
  size <- merge(x=expand.grid(lapply(x[stratanames], unique)),
                y=twinsize, all.x=TRUE, all.y=TRUE)
  size$Freq[is.na(size$Freq)] <- 0
  
  s <- Strata(x = x, stratanames = stratanames, size=size$Freq, method=method, 
              pik=pik, description=description)
  
  if(!identical(table(s[,stratanames]), table(twins[,stratanames]))) {
    warning("Could not find a twin for all records. Enlighten the restrictions!")
  }
  return(s)
  
}
