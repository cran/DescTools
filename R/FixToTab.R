FixToTab <-
function(txt, sep = " ", delim = "\t", trim = TRUE, header = TRUE){

  # converts a fixed text to a delim separated table

  m <- do.call("rbind", strsplit(txt, ""))
  
  idx <- apply( m, 2, function(x) all(x == sep))
  # replace all multiple delims by just one
  idx[-1][(apply(cbind(idx[-1], idx[-length(idx)]), 1, sum) == 2)] <- FALSE
  m[,idx] <- delim
  tab <- apply( m, 1, paste, collapse="")

  # trim the columns
  if(trim) { 
    tab <- do.call("rbind", lapply(strsplit(tab, delim), StrTrim))
  } else {
    tab <- do.call("rbind", strsplit(tab, delim))
  }  
  
  if(header) {
    colnames(tab) <- tab[1,]
    tab <- tab[-1,]
  }
  
  return(tab)
  
}
