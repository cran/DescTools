SaveAs <-
function(x, filename){
  if(missing(filename))
    filename <- file.choose()
  if(! is.na(filename)) save(list=deparse(substitute(x)), file = filename)
  else 
    warning("No filename supplied")
}
