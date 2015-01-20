ImportDlg <-
function(fmt=1) {
  # read.table text:
  if(fmt == 1) {
    fmt <- "\"%path%%fname%.%fxt%\""
  } else { if( fmt ==2) {   
    fmt="d.%fname% <- read.table(file = \"%path%%fname%.%fxt%\", header = TRUE, sep = \";\", na.strings = c(\"NA\",\"NULL\"), strip.white = TRUE)"
  }}

  # fn <- file.choose()
  fn <- tclvalue(tkgetOpenFile()) 
  
  op <- options(useFancyQuotes = FALSE)
  # switch from backslash to slash
  fn <- gsub("\\\\", "/", fn)
  
  # parse the filename into path, filename, filextension
  fnamelong <- rev(unlist(strsplit(fn, "/")))[1]
  fxt <- rev(unlist(strsplit( fnamelong, "\\.")))[1]
  fname <- substr(fnamelong, 1, nchar(fnamelong) - nchar(fxt) - 1) 
  path <- substr(fn, 1, nchar(fn) - nchar(fname) - nchar(fxt) - 1) 

  rcmd <- gsub("%fname%", fname, gsub("%fxt%", fxt, gsub( "%path%", path, fmt)))

  # utils::writeClipboard(rcmd)
  .writeCB(rcmd)
  
  options(op)

  invisible(rcmd) 
  
}
