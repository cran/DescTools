StrPos <-
function(x, pattern, pos=1, ... ){

# example:  
#    StrPos(x=levels(d.pizza$driver), "t", pos=4)
  
  pos <- rep(pos, length.out=length(x))
  x <- substr(x, start=pos, stop=nchar(x))
  
  i <- as.vector(regexpr(pattern = pattern, text = x, ...))
  i[i<0] <- NA
  return(i)
}
