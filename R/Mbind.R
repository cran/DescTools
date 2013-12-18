Mbind <-
function(...){
  # matrix bind
  # function um n nxm-matrizen zu einem 3d-array zusammenzufassen
  
  arg.list <- list(...)
  # check dimensions, by compare the dimension of each matrix to the first
  if( !all( unlist(lapply(arg.list, function(m) all(unlist(dim(arg.list[[1]])) == unlist(dim(m)))) )))
     stop("Not all matrices have the same dimension!")
    
  ma <- array(unlist(arg.list), dim=c(nrow(arg.list[[1]]), ncol(arg.list[[2]]), length(arg.list)) )
  dimnames(ma) <- dimnames(arg.list[[1]])
  dimnames(ma)[[3]] <- if(is.null(names(arg.list))){1:length(arg.list)} else {names(arg.list)}

  return(ma)
}
