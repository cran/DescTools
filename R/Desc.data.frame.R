Desc.data.frame <-
function(x, sep=paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), 
                            main = NULL, enum = TRUE, ...) {
  
  cat( "\n", sep, "\n", sep="" )
  cat( capture.output(Str(x, list.len=Inf)), sep="\n")    # Overview
  cat( "\n")
  # Alternative mit Fensterbreite:
  # cat( paste(rep("-",getOption("width")-2),collapse=""), "\n")
  
  if(is.null(main))
    main <- paste(if(enum) paste(seq_along(colnames(x)) , "- "), colnames(x),
                  " (", lapply(lapply(x, class), paste, collapse=", "), ")", sep="")
  else
    main <- rep(main, length.out = ncol(x))
  
  for( cx in colnames(x) ){
    Desc(x[, cx], main = main[match(cx, colnames(x))], ...)
  }
  cat("\n")
  invisible()

  
}
