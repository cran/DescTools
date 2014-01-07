StrCap <-
function(x) {
  # Source: Hmisc
  # Author: Charles Dupont
  capped <- grep('^[^A-Z]*', x, perl=TRUE)

  substr(x[capped], 1,1) <- toupper(substr(x[capped], 1,1))
  return(x)
  
}
