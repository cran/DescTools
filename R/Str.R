Str <-
function(x, ...){
  if(class(x)=="data.frame"){
    out <- capture.output(str(x, ...))
    idx <- format(1:length(grep(pattern="^ \\$", out)))
    i <- 1
    j <- 1
    while(i <= length(out)) {
      if( length(grep(pattern="^ \\$", out[i])) > 0 ) {
        out[i] <- gsub(pattern="^ \\$", replacement= paste(" ", idx[j], " \\$", sep=""), out[i]) 
        j <- j + 1
      }
      i <- i + 1
    }  
    res <- out
  } else {
    res <- str(x)
  }
  cat(res, sep="\n")
  invisible(res)
}
