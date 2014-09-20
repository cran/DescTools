IsValidWrd <-
function(wrd = getOption("lastWord")){
  # returns TRUE if the selection of the wrd pointer can be evaluated
  # meaning the pointer points to a running word instance and so far valid
  res <- tryCatch(wrd[["Selection"]], error=function(e) {e})  
  return(!inherits(res, "simpleError")) # Error in 
    
}
