InDots <-
function(..., arg, default){

  # was arg in the dots-args? parse dots.arguments
  arg <- unlist(match.call(expand.dots=FALSE)$...[arg])

  # if arg was not in ... then return default
  if(is.null(arg)) arg <- default
  
  return(arg)
  
}
