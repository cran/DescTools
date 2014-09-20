DescWrd.data.frame <-
function(x, wrd, ...){

  # Start report:     data.frame  Infos einfügen **************
  WrdCaption( "Describe data.frame", wrd=wrd )  
  wrd[["Selection"]]$TypeParagraph()
  WrdText( capture.output( Str(x) ), wrd=wrd )
  wrd[["Selection"]]$TypeParagraph()
  wrd[["Selection"]]$TypeParagraph()

  for( cx in colnames(x) ){
    # cat( "\n", sep, "\n", sep="") 
    # Alternative mit Fensterbreite:
    # cat( paste(rep("-",getOption("width")-2),collapse=""), "\n")
    main <- paste( match(cx, colnames(x)), " - ", cx, " (", paste(class(x[,cx]), collapse=" "),")", sep="" )
    Desc( x[,cx], main = main, wrd = wrd, ...)
  }
  cat("\n")
  invisible()
  
}
