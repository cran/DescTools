Strata <-
function (x, stratanames = NULL, size = 1, 
                    method = c("srswor", "srswr", "poisson", "systematic"), 
                    pik, description = FALSE) {
  
  method <- match.arg(method, c("srswor", "srswr", "poisson", "systematic"))
  
  # find non factors in stratanames
  factor_fg <- unlist(lapply(x[, stratanames, drop=FALSE], is.factor))
  # factorize nonfactors, get their levels and combine with levels of existing factors
  lvl <- c(lapply(lapply(x[,names(which(!factor_fg)), drop=FALSE], factor), levels)
           , lapply(x[,names(which(factor_fg)), drop=FALSE], levels))
  
  # get the stratanames in the given order 
  strat <- expand.grid(lvl[stratanames])
  strat$stratum <- factor(1:nrow(strat))
  
  # set the size for the strata to sample
  strat$size <- rep(size, length.out=nrow(strat))
  
  # prepare the sample
  x <- merge(x, strat)
  x$id <- 1:nrow(x)
  n <- table(x$stratum)
  
  if(method %in% c("srswor", "srswr")) {
    res <- do.call(rbind,
                   lapply(split(x, x$stratum), 
                          function(z){
                            if(nrow(z)>0){
                              idx <- sample(x=nrow(z), size=z$size[1], replace=(method=="srswr"))
                              z[idx,] 
                            } else {
                              z
                            }   
                          }
                   )  
    )
  } else if(method == "poisson") {
    
    # still to implement!!!  *********************
    res <- do.call(rbind,
                   lapply(split(x, x$stratum), 
                          function(z){
                            if(nrow(z)>0){
                              idx <- sample(x=nrow(z), size=z$size[1], replace=(method=="srswr"))
                              z[idx,] 
                            } else {
                              z
                            }   
                          }
                   )  
    )               
  } else if(method == "systematic") {

    # still to implement!!!  *********************
    res <- do.call(rbind,
                   lapply(split(x, x$stratum), 
                          function(z){
                            if(nrow(z)>0){
                              idx <- sample(x=nrow(z), size=z$size[1], replace=(method=="srswr"))
                              z[idx,] 
                            } else {
                              z
                            }   
                          }
                   )  
    )              
  }  
  
  return(res)
  
}
