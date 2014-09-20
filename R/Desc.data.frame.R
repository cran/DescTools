Desc.data.frame <-
function(x, sep=paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), ...) {

  cat( "\n", sep, "\n", sep="" )
  cat( capture.output(Str(x, list.len=Inf)), sep="\n")		# Overview
  cat( "\n")
  for( cx in colnames(x) ){
    # cat( "\n", sep, "\n", sep="") 
    # Alternative mit Fensterbreite:
    # cat( paste(rep("-",getOption("width")-2),collapse=""), "\n")
    main <- paste( match(cx, colnames(x)), " - ", cx, " (", paste(class(x[,cx]), collapse=" "),")", sep="" )
    Desc( x[,cx], main = main, ...)
  }
  cat("\n")
  invisible()
  
}
