ParseSASDatalines <-
function(x, env = .GlobalEnv, overwrite = FALSE) {
  
  # see: http://www.psychstatistics.com/2012/12/07/using-datalines-in-sas/
  # or:  http://www.ats.ucla.edu/stat/sas/library/SASRead_os.htm

  # split command to list by means of ;
  lst <- StrTrim(strsplit(x, ";")[[1]])
  dsname <- lst[grep(pattern = "^[Dd][Aa][Tt][Aa] ", StrTrim(lst))]   # this would be the dataname
  dsname <- gsub(pattern = "^[Dd][Aa][Tt][Aa] +", "", dsname)
  
  # get the columnnames from the input line
  input <- lst[grep(pattern = "^[Ii][Nn][Pp][Uu][Tt]", StrTrim(lst))]
  # get rid of potential single @
  input <- gsub("[ \n\t]@+[ \n\t]*", "", input)
  input <- gsub(pattern=" +\\$", "$", input)
  input <- gsub(" +", " ", input)
  cnames <- strsplit(input, " ")[[1]][-1]
  
  # the default values for the variables
  def <- rep(0, length(cnames))
  def[grep("\\$$", cnames)] <- "''"
  vars <- paste(gsub("\\$$","",cnames), def, sep="=", collapse=",")
  
  datalines <- lst[grep("datalines", tolower(lst))+1]
  
  res <- eval(parse(text=gettextf(
    "data.frame(scan(file=textConnection(datalines), 
    what=list(%s), quiet=TRUE))", vars)))              
  
  if(length(dsname) > 0){ # check if a dataname could be found
    if( overwrite | ! exists(dsname, envir=env) ) {
      assign(dsname, res, envir=env)
    } else {
      stop(gettextf("%s already exists in %s. Use overwrite = TRUE to overwrite it.", dsname, deparse(substitute(env))))
    } 
  }
  return(res)

}
