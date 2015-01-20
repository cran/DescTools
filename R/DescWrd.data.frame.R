DescWrd.data.frame <-
function (x, wrd, main = NULL, enum = TRUE, ...) {
    
  # Start report:     data.frame  Infos einfügen **************
  WrdCaption( "Describe data.frame", wrd=wrd )  
  wrd[["Selection"]]$TypeParagraph()
  WrdText(capture.output(Str(x, list.len = Inf)), wrd = wrd)
  wrd[["Selection"]]$TypeParagraph()
  wrd[["Selection"]]$TypeParagraph()

  if(is.null(main))
    main <- paste(if(enum) paste(seq_along(colnames(x)) , "- "), colnames(x),
                  " (", lapply(lapply(x, class), paste, collapse=", "), ")", sep="")
  else
    main <- rep(main, length.out = ncol(x))

  for( cx in colnames(x) ){
    # cat( "\n", sep, "\n", sep="") 
    # Alternative mit Fensterbreite:
    # cat( paste(rep("-",getOption("width")-2),collapse=""), "\n")
    Desc(x[, cx], main = main[match(cx, colnames(x))], wrd = wrd, ...)
  }
  cat("\n")
  invisible()
  
}
