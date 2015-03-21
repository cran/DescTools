StrTrunc <-
function(x, maxlen = 20) {

  # original truncString from prettyR
  # author: Jim Lemon
    
  #   toolong <- nchar(x) > maxlen
  #   maxwidth <- ifelse(toolong, maxlen - 3, maxlen)
  #   chopx <- substr(x, 1, maxwidth)
  # 
  #   for(i in 1:length(x)) if(toolong[i]) chopx[i] <- paste(chopx[i], "...", sep="")
  # 
  #   return(formatC(chopx, width = maxlen, flag = ifelse(justify == "left", "-", " ")) )

  # ... but this is all a bit clumsy, let's have it shorter:  ;-)
  
  paste(substr(x, 0, maxlen), ifelse(nchar(x) > maxlen, "...", ""), sep="")
}
