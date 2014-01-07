Rename <-
function(x, ..., gsub=FALSE, fixed=TRUE, warn=TRUE){

  subst <- c(...)
  if(gsub){
    names.x <- names(x)
    for(i in 1:length(subst)){
      names.x <- gsub(names(subst[i]),subst[i],names.x,fixed=fixed)
    }
    names(x) <- names.x
  }
  else {
    i <- match(names(subst),names(x))
    if(any(is.na(i))) {
      if(warn) warning("unused name(s) selected")
      if(any(!is.na(i)))
        subst <- subst[!is.na(i)]
      i <- i[!is.na(i)]
    }
    if(length(i))
      names(x)[i] <- subst
  }
  return(x)
}
