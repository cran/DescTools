StrPad <-
function(x, len, str = " ", adj = c("left", "right", "center")) {
  mto <- match.arg(adj)
  free  <- len - nchar(x)
  fill   <- substring(paste(rep(str, ceiling(free / nchar(str))), collapse = ""), 1, free)
  #### cat("  free=",free,",  fill=",fill,",  mto=",mto,"\n")
  if(free <= 0) substr(x, 1, len)
    else if  (mto == "left") paste(x, fill, sep = "")
    else if  (mto == "right") paste(fill, x, sep = "")
    else  paste(substring(fill, 1, free %/% 2), x, substring(fill, 1 + free %/% 2, free), sep = "")
}
