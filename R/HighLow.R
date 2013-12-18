HighLow <-
function(x, nlow = 5, nhigh = nlow){
  
  x <- na.omit(x)
  
  tab <- table(x)
  
  nlow <- min(length(tab), nlow)
  nhigh <- min(length(tab), nhigh)
  
  if((nlow+nhigh)!=0){
    tab <- tab[unique(c(1:nlow, (length(tab)-nhigh):length(tab)))]
    if (is.numeric(x)) {
      # prettyNum much slower than sprintf, but sprintf has no big.mark
      # vals <- sprintf(as.numeric(names(tab)), big.mark = "'")
      vals <- prettyNum(as.numeric(names(tab)), big.mark = "'")
    } else {
      vals <- names(tab)
    }
    frqtxt <- paste(" (", tab, ")", sep="")
    frqtxt[tab < 2] <- "" 
  
    txt <- StrTrim(paste( vals, frqtxt, sep=""))
    lowtxt <- paste(head(txt, nlow), collapse=", ")
    hightxt <- paste(tail(txt, nhigh), collapse=", ")

  } else {
    lowtxt <- ""
    hightxt <- ""
    
  }
  return( paste("lowest : ", lowtxt, "\n", "highest: ", hightxt, "\n", sep="") )
  
}
