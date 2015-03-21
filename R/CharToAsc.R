CharToAsc <-
function(x) { 
  # Original from Henrik Bengtsson R.oo:
  # char2asc <- function (ch, ...) { match(ch, ASCII) - 1 }
  # example:  x.char <- char2asc(x="Andri")


  if(length(x) == 1) 
    strtoi(charToRaw(x), 16L)
  else
    sapply(x, function(x) strtoi(charToRaw(x), 16L))
  
}
