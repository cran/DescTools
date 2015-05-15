WrdTable.ftable <-
function(tab, main = NULL, wrd = getOption("lastWord"), row.names = FALSE, ...) {
  tab <- FixToTab(capture.output(tab))
  NextMethod()
}
