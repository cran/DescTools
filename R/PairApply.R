PairApply <-
function(x, FUN = NULL, ..., symmetric = TRUE){
  
  if(is.function(FUN)) { 
    # if FUN is a function, then save it under new name and 
    # overwrite function name in FUN, which has to be character
    fct <- FUN
    FUN <- "fct"
  }
  
  if(is.matrix(x)) x <- as.data.frame(x)
  x <- as.list(x)
  
  ix <- 1:length(x)
  # pairwise logic from pairwise.table
  pp <- outer(ix, ix, function(ivec, jvec) sapply(seq_along(ivec), 
                                                  function(k) {
                                                    i <- ivec[[k]]
                                                    j <- jvec[[k]]
                                                    if (i > j) 
                                                      eval(parse(text = gettextf("%s(x[[i]], x[[j]], ...)", FUN))) 
                                                    else NA
                                                  }))
  diag(pp) <- 1
  if(symmetric){
    pp[upper.tri(pp)] <- t(pp)[upper.tri(t(pp))]
  } else {
    pp.upr <- outer(ix, ix, function(ivec, jvec) sapply(seq_along(ivec), 
                                                        function(k) {
                                                          i <- ivec[[k]]
                                                          j <- jvec[[k]]
                                                          if (i > j) 
                                                            eval(parse(text = gettextf("%s(x[[j]], x[[i]], ...)", FUN))) 
                                                          else NA
                                                        }))
    pp[upper.tri(pp)] <- t(pp.upr)[upper.tri(pp.upr)]
    
  }  

  dimnames(pp) <- list(names(x),names(x))
  
  return(pp)
}
