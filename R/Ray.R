Ray <-
function(x){
  idx <- WhichNumerics(x)
  nums <- data.frame(
    idx=match( idx, names(x)), 
    classes=do.call("rbind", lapply(x[,idx], class)), 
    typeof=do.call("rbind", lapply(x[,idx], typeof)), 
    mode=do.call("rbind", lapply(x[,idx], mode)), 
    NAs=do.call("rbind", lapply(x[,idx], function(x) sum(is.na(x)))), 
    mean=do.call("rbind", lapply(x[,idx], mean, na.rm=TRUE)), 
    sd=do.call("rbind", lapply(x[,idx], sd, na.rm=TRUE)), 
    median=do.call("rbind", lapply(x[,idx], median, na.rm=TRUE)), 
    IQR=do.call("rbind", lapply(x[,idx], IQR, na.rm=TRUE)), 
    min=do.call("rbind", lapply(x[,idx], min, na.rm=TRUE)), 
    max=do.call("rbind", lapply(x[,idx], max, na.rm=TRUE)) 
  )
  
  idx <- WhichFactors(x)
  facts <- data.frame(
    idx=match( idx, names(x)), 
    classes=do.call("rbind", lapply(x[,idx], function(x) paste(class(x), collapse=", "))), 
    typeof=do.call("rbind", lapply(x[,idx], typeof)), 
    mode=do.call("rbind", lapply(x[,idx], mode)), 
    NAs=do.call("rbind", lapply(x[,idx], function(x) sum(is.na(x)))), 
    nlevels=do.call("rbind", lapply(x[,idx], nlevels))
  )
  
  idx <- colnames(x)[colnames(x) %nin% c(WhichNumerics(x), WhichFactors(x))]
  elses <- data.frame(
    idx=match( idx, names(x)), 
    classes=do.call("rbind", lapply(x[,idx], class)), 
    typeof=do.call("rbind", lapply(x[,idx], typeof)), 
    mode=do.call("rbind", lapply(x[,idx], mode)), 
    NAs=do.call("rbind", lapply(x[,idx], function(x) sum(is.na(x)))) 
  )
  
  return(list("numeric"=nums, "factors"=facts, "rest"=elses))
  
}
