ClipToVect <-
function(doubleQuote = TRUE){

  # vectorizes the clipboard content
    
  x <- read.table("clipboard", sep="\t")

  if(!all(sapply(x, is.numeric))){
    if(doubleQuote)x <- sapply(x, dQuote) 
    else x <- sapply(x, sQuote)  
  }

  res <- paste(apply(x, 2, function(x) paste("c(", paste(x, collapse=",") , ")", sep="")), collapse=",\n")
         
  
  cat(res)
  invisible(res)
  
}
