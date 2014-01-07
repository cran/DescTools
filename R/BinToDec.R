BinToDec <-
function(x) { 
  # Alternative:  bin2dec <- function(x) { sum(2^.subset((length(x)-1):0, x)) } 
  # example: bin2dec(x=as.numeric(unlist(strsplit("1001", split=NULL)))==1)
  strtoi(x, 2L)
}
