Ray <-
function(x){
  nidx <- sort(c(which(sapply(x, inherits, "numeric")), which(sapply(x, inherits, "integer")))  )
  nums <- data.frame(
    idx= nidx, 
    classes=do.call("rbind", lapply(x[,nidx], class)), 
    typeof=do.call("rbind", lapply(x[,nidx], typeof)), 
    mode=do.call("rbind", lapply(x[,nidx], mode)), 
    NAs=do.call("rbind", lapply(x[,nidx], function(x) sum(is.na(x)))), 
    mean=do.call("rbind", lapply(x[,nidx], mean, na.rm=TRUE)), 
    sd=do.call("rbind", lapply(x[,nidx], sd, na.rm=TRUE)), 
    median=do.call("rbind", lapply(x[,nidx], median, na.rm=TRUE)), 
    IQR=do.call("rbind", lapply(x[,nidx], IQR, na.rm=TRUE)), 
    min=do.call("rbind", lapply(x[,nidx], min, na.rm=TRUE)), 
    max=do.call("rbind", lapply(x[,nidx], max, na.rm=TRUE)) 
  )
  
  fidx <- which(sapply(x, is.factor))
  facts <- data.frame(
    idx=fidx, 
    classes=do.call("rbind", lapply(x[,fidx], function(x) paste(class(x), collapse=", "))), 
    typeof=do.call("rbind", lapply(x[,fidx], typeof)), 
    mode=do.call("rbind", lapply(x[,fidx], mode)), 
    NAs=do.call("rbind", lapply(x[,fidx], function(x) sum(is.na(x)))), 
    nlevels=do.call("rbind", lapply(x[,fidx], nlevels))
  )
  
  idx <- seq_along(x)[-c(nidx, fidx)]
  elses <- data.frame(
    idx=idx, 
    classes=do.call("rbind", lapply(x[,idx], class)), 
    typeof=do.call("rbind", lapply(x[,idx], typeof)), 
    mode=do.call("rbind", lapply(x[,idx], mode)), 
    NAs=do.call("rbind", lapply(x[,idx], function(x) sum(is.na(x)))) 
  )
  
  return(list("numeric"=nums, "factors"=facts, "rest"=elses))
  
}
