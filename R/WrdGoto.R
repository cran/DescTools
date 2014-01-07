WrdGoto <-
function (name, what = wdConst$wdGoToBookmark, wrd = getOption("lastWord")) {
  wrdSel <- wrd[["Selection"]]
  wrdSel$GoTo(what=what, Name=name)
  invisible()
}
