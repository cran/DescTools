Desc.list <-
function(x, sep=paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), 
                      main = NULL, enum = TRUE, ...) {

  cat("\n", sep, "\n", sep="")
  cat(capture.output(Str(x, list.len=Inf)), sep="\n")  	# Overview
  cat("\n")
  
  if(is.null(main))
    main <- paste(if(enum) paste(seq_along(names(x)) , "- "), names(x),
                  " (", lapply(lapply(x, class), paste, collapse=", "), ")", sep="")
  else
    main <- rep(main, length.out = length(x))
  
  for( i in 1:length(x) ){
    Desc(x[[i]], main=main[i], ...)
  }
  cat("\n")
  invisible()
  
}
