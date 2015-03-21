DescWrd.list <-
function (x, wrd, main = NULL, enum = TRUE, ...) {
  
  # Start report:     data.frame  Infos einfuegen **************
  WrdCaption( "Describe data.frame", wrd=wrd )  
  wrd[["Selection"]]$TypeParagraph()
  WrdText(.CaptOut(Str(x, list.len = Inf)), wrd = wrd)
  wrd[["Selection"]]$TypeParagraph()
  wrd[["Selection"]]$TypeParagraph()
  
  if(is.null(main))
    main <- paste(if(enum) paste(seq_along(names(x)) , "- "), names(x),
                  " (", lapply(lapply(x, class), paste, collapse=", "), ")", sep="")
  else
    main <- rep(main, length.out = length(x))
  
  for( i in seq_along(x) ){
    # cat( "\n", sep, "\n", sep="") 
    # Alternative mit Fensterbreite:
    # cat( paste(rep("-",getOption("width")-2),collapse=""), "\n")
    Desc(x[, i], main = main[i], wrd = wrd, ...)
  }
  cat("\n")
  invisible()
  
}
