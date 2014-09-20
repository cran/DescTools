WrdTable.default <-
function (tab,  wrd = getOption("lastWord"), row.names=FALSE, ...) {
  
  
  dim1 <- ncol(tab)
  dim2 <- nrow(tab)
  if(row.names) dim1 <- dim1 + 1
  
  # wdConst ist ein R-Objekt (Liste mit 2755 Objekten!!!)
  
  write.table(tab, file = "clipboard", sep = "\t", quote = FALSE, row.names=row.names)
  
  myRange <- wrd[["Selection"]][["Range"]]
  bm      <- wrd[["ActiveDocument"]][["Bookmarks"]]$Add("PasteHere", myRange)
  myRange$Paste()
  
  if(row.names) wrd[["Selection"]]$TypeText("\t")
  
  myRange[["Start"]] <- bm[["Range"]][["Start"]]
  myRange$Select()
  bm$Delete()
  wrd[["Selection"]]$ConvertToTable(Separator       = wdConst$wdSeparateByTabs, 
                                    NumColumns      = dim1,                             
                                    NumRows         = dim2, 
                                    AutoFitBehavior = wdConst$wdAutoFitFixed)
  
  wrdTable <- wrd[["Selection"]][["Tables"]]$Item(1)
  # http://www.thedoctools.com/downloads/DocTools_List_Of_Built-in_Style_English_Danish_German_French.pdf
  wrdTable[["Style"]] <- -115 # "Tabelle Klassisch 1"
  wrdSel <- wrd[["Selection"]]
  wrdSel[["ParagraphFormat"]][["Alignment"]] <- wdConst$wdAlignParagraphLeft
  
  wrdTable[["Columns"]]$Item(1)$Select()
  wrd[["Selection"]][["ParagraphFormat"]][["Alignment"]] <- wdConst$wdAlignParagraphLeft
  
  # Cursor aus der Tabelle auf die letzte Postition im Dokument setzten
  # Selection.GoTo What:=wdGoToPercent, Which:=wdGoToLast
  wrd[["Selection"]]$GoTo(What = wdConst$wdGoToPercent, Which= wdConst$wdGoToLast)
  
  invisible()
  
}
