WrdUpdateBookmark <-
function (name, text, what = wdConst$wdGoToBookmark, wrd = getOption("lastWord")) {
  
  #   With ActiveDocument.Bookmarks
  #   .Add Range:=Selection.Range, Name:="entb"
  #   .DefaultSorting = wdSortByName
  #   .ShowHidden = False
  #   End With
  
  wrdSel <- wrd[["Selection"]]
  wrdSel$GoTo(What=what, Name=name)
  wrdSel[["Text"]] <- text
  # the bookmark will be deleted, how can we avoid that?
  wrdBookmarks <- wrd[["ActiveDocument"]][["Bookmarks"]]
  wrdBookmarks$Add(name)
  invisible()
}
