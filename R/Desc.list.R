Desc.list <-
function(x, sep=paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), ...) {
#   if(is.null(main)) main <- names(x)
#   for(i in 1:length(x)){
#     Desc(x[[i]], main=main[i], ...)
#   }  
  
  cat( "\n", sep, "\n", sep="" )
  cat( capture.output(Str(x, list.len=Inf)), sep="\n")  	# Overview

  for( i in 1:length(x) ){
    # cat( "\n", sep, "\n", sep="") 
    # Alternative mit Fensterbreite:
    # cat( paste(rep("-",getOption("width")-2),collapse=""), "\n")
    main <- paste( i, " - ", names(x)[i], " (", paste(class(x[[i]]), collapse=" "),")", sep="" )
    Desc(x[[i]], main=main, ...)
  }
  cat("\n")
  invisible()
  
}
