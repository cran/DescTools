StrVal <-
function(x, paste = FALSE, as.numeric = FALSE){
  
  # Problem 20.2.2015: - will not be accepted, when a space is between sign and number
  # not sure if this is really a problem: -> oberserve...
  # StrVal(x="- 2.5", paste = FALSE, as.numeric = FALSE)
  
  pat <- "[-+.e0-9]*\\d" 
  gfound <- gregexpr(pattern=pat, text=x)
  vals <- lapply(seq_along(x), function(i){
    found <- gfound[[i]]
    ml <- attr(found, which="match.length")
    res <- sapply(seq_along(found), function(j) substr(x[i], start=found[j], stop=found[j]+ml[j]-1) )
    return(res)
  })
  
  if(paste==TRUE) {
    vals <- sapply(vals, paste, collapse="")
    if(as.numeric==TRUE)
      vals <- as.numeric(vals)
  } else {
    if(as.numeric==TRUE)
      vals <- lapply(vals, as.numeric)
  }
  
  return(vals)
  
}
