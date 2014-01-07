WrdInsertBookmark <-
function (name, wrd = getOption("lastWord")) {
  
  #   With ActiveDocument.Bookmarks
  #   .Add Range:=Selection.Range, Name:="entb"
  #   .DefaultSorting = wdSortByName
  #   .ShowHidden = False
  #   End With
  
  wrdBookmarks <- wrd[["ActiveDocument"]][["Bookmarks"]]
  wrdBookmarks$Add(name)
  invisible()
}
