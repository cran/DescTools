CharToAsc <-
function(x) { 
  # Original from Henrik Bengtsson R.oo:
  # char2asc <- function (ch, ...) { match(ch, ASCII) - 1 }
  # example:  x.char <- char2asc(x="Andri")

  # ASCII <- intToUtf8(1:256, multiple=TRUE)
  # ASCII[128:159] <- c("€","","‚","ƒ","„","…","†","‡","ˆ","‰","Š","‹","Œ","","Ž","","","‘","’","“","”","•","–","—","˜","™","š","›","œ","","ž","Ÿ")
  # return( as.vector(sapply(strsplit(x,NULL), match, ASCII) - 1 ))
  
  strtoi(charToRaw(x), 16L) 
}
