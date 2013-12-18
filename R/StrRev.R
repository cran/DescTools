StrRev <-
function(x) {
  # reverses a string
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="") 
}
