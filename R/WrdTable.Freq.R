WrdTable.Freq <-
function(tab, main = NULL, wrd = getOption("lastWord"), row.names = FALSE, ...){
  
  tab[,c(3,5)] <- sapply(round(tab[,c(3,5)], 3), Format, digits=3)
  WrdTable.default(tab=tab, wrd=wrd)
  
  if(!is.null(main)){
    # insert caption
    sel <- wrd$Selection()  # "Abbildung"
    sel$InsertCaption(Label=wdConst$wdCaptionTable, Title=main)
    sel$TypeParagraph()
  }

  invisible()
  
}
