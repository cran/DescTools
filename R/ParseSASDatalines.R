ParseSASDatalines <-
function(x){
  
  # split command to list by means of ;
  lst <- StrTrim(strsplit(x, ";")[[1]])
  # main <- lst[1]   # this would be the dataname
  
  # get the columnnames from the input line
  input <- lst[grep(pattern = "^[Ii][Nn][Pp][Uu][Tt]", StrTrim(lst))]
  # get rid of potential single @
  input <- gsub("[ \n\t]@+[ \n\t]*", "", input)
  cnames <- strsplit(gsub(pattern=" +\\$", "$", input), " ")[[1]][-1]
  
  # the default values for the variables
  def <- rep(0, length(cnames))
  def[grep("\\$$", cnames)] <- "''"
  vars <- paste(gsub("\\$$","",cnames), def, sep="=", collapse=",")
  
  datalines <- lst[grep("datalines", tolower(lst))+1]
  
  res <- eval(parse(text=gettextf(
    "data.frame(scan(file=textConnection(datalines), 
    what=list(%s), quiet=TRUE))", vars)))              
  
  return(res)
  
}
