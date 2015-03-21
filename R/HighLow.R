HighLow <-
function (x, nlow = 5, nhigh = nlow, na.rm = FALSE) {
  
  # updated 1.2.2014 / Andri
  # using table() was unbearable slow and inefficient for big vectors!!
  # sort(partial) is the way to go..
  # http://r.789695.n4.nabble.com/Fast-way-of-finding-top-n-values-of-a-long-vector-td892565.html
  
  if(na.rm) x <- na.omit(x)
  
  if ((nlow + nhigh) != 0) {
    frqs <- Small(x, k=nlow, unique=TRUE, na.rm=na.rm)
    frql <- Large(x, k=nhigh, unique=TRUE, na.rm=na.rm)
    frq <- c(frqs$lengths, frql$lengths)
    
    vals <- c(frqs$values, frql$values)
    if (is.numeric(x)) {
      vals <- prettyNum(vals, big.mark = "'")
    }
    else {
      vals <- vals
    }
    frqtxt <- paste(" (", frq, ")", sep = "")
    frqtxt[frq < 2] <- ""
    
#     txt <- StrTrim(paste(vals, frqtxt, sep = ""))
#     lowtxt <- paste(head(txt, nlow), collapse = ", ")
#     hightxt <- paste(tail(txt, nhigh), collapse = ", ")

    txt <- StrTrim(paste(vals, frqtxt, sep = ""))
    lowtxt <- paste(head(txt, min(length(frqs$lengths), nlow)), collapse = ", ")
    hightxt <- paste(tail(txt, min(length(frql$lengths), nhigh)), collapse = ", ")
  }
  else {
    lowtxt <- ""
    hightxt <- ""
  }
  return(paste("lowest : ", lowtxt, "\n", 
               "highest: ", hightxt, "\n", sep = ""))
}
