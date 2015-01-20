Untable.data.frame <-
function(x, freq = "Freq", rownames = NULL, ...){
  
  if(all(is.na(match(freq, names(x))))) 
    stop(gettextf("Frequency column %s does not exist!", freq))

  res <- x[Untable(x[,freq], type="as.numeric")[,], -grep(freq, names(x))]
  rownames(res) <- rownames
  
  return(res)
}
