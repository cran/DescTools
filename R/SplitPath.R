SplitPath <-
function(path, last.is.file=NULL) {
  
  if(is.null(last.is.file)){
    # if last sign is delimiter / or \ read path as dirname
    last.is.file <- (length(grep(pattern="[/\\]$", path)) == 0)
  } 

  path <- normalizePath(path, mustWork = FALSE)
  
  lst <- list()
  
  lst$normpath <- path
  if (.Platform$OS.type == "windows") {
    lst$drive <- regmatches(path, regexpr("^([[:alpha:]]:)|(\\\\[[:alnum:]]+)", path))
    lst$dirname <- gsub(pattern=lst$drive, x=dirname(path), replacement="")
  } else {
    lst$drive <- NA
    lst$dirname <- dirname(path)
  }
  
  lst$dirname <- paste(lst$dirname, "/", sep="")
  lst$fullfilename <- basename(path)

  lst$filename <- strsplit(lst$fullfilename, "\\.")[[1]][1]
  lst$extension <- strsplit(lst$fullfilename, "\\.")[[1]][2]

  if(!last.is.file){
    lst$dirname <- paste(lst$dirname, lst$fullfilename, "/",
                         sep="")
    lst$extension <- lst$filename <- lst$fullfilename <- NA
  }  
  return(lst)
  
}
