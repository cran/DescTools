ChooseColorDlg <-
function() {
  requireNamespace("tcltk", quietly = FALSE)
  return(as.character(tcltk::tcl("tk_chooseColor", title="Choose a color")))
}
