StrVal <-
function(x){
  
  pat <- "[-+.e0-9]*\\d" 
  gfound <- gregexpr(pattern=pat, text=x)
  vals <- sapply(seq_along(x), function(i){
    found <- gfound[[i]]
    ml <- attr(found, which="match.length")
    res <- sapply(seq_along(found), function(j) substr(x[i], start=found[j], stop=found[j]+ml[j]-1) )
    return(res)
  })
  
  return(vals)
}
