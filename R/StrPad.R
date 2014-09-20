StrPad <-
function(x, len, pad = " ", adj = c("left", "right", "center")) {
  
  .pad <- function(x, len, pad=" ", adj=c("left", "right", "center")){
    
    if(is.na(x)) return(NA)
    
    mto <- match.arg(adj)
    free <- len - nchar(x)
    fill <- substring(paste(rep(pad, ceiling(free / nchar(pad))), collapse = ""), 1, free)
    #### cat("  free=",free,",  fill=",fill,",  mto=",mto,"\n")
    if(free <= 0) substr(x, 1, len)
    else if  (mto == "left") paste(x, fill, sep = "")
    else if  (mto == "right") paste(fill, x, sep = "")
    else  paste(substring(fill, 1, free %/% 2), x, substring(fill, 1 + free %/% 2, free), sep = "")
  }
  
  adj <- sapply(adj, match.arg, choices=c("left", "right", "center"))
  
  lgp <- Recycle(x=x, len=len, pad=pad, adj=adj)
  sapply( 1:attr(lgp, "maxdim"), function(i) .pad(lgp$x[i], lgp$len[i], lgp$pad[i], lgp$adj[i]) )
  
}
