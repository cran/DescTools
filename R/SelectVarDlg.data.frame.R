SelectVarDlg.data.frame <-
function(x, ...) { 
  txt <- paste( deparse(substitute(x)), "[,", SelectVarDlg.default( x = colnames(x), ...), "]", sep="", collapse="") 
  writeClipboard(txt)
  invisible(txt)
}
