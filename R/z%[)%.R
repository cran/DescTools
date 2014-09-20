`%[)%` <-
function(x, rng) { 
  
  if(is.matrix(rng)){
    # recycle things
    # which parameter has the highest dimension
    maxdim <- max(length(x), nrow(rng))
    # recycle all params to maxdim
    x <- rep(x, length.out = maxdim)
    # the rows of the matrix rng
    rng <- rng[rep(1:nrow(rng), length.out = maxdim),]
    
    res <- .Call("between_num_lm", as.numeric(x), as.numeric(rng[,1]), as.numeric(rng[,2]), PACKAGE="DescTools")
    res[is.na(x)] <- NA
    
    return( res)
    
  }
    
  if(is.numeric(x)) {
    # as.numeric still needed for casting integer to numeric!!
    res <- .Call("between_num_l", as.numeric(x), as.numeric(rng[1]), as.numeric(rng[2]), PACKAGE="DescTools")
    res[is.na(x)] <- NA
  } else if(is.ordered(x)) { 
    res <- .Call("between_num_l", as.numeric(x), as.numeric(match(rng[1], levels(x))), as.numeric(match(rng[2], levels(x))), PACKAGE="DescTools")
    res[is.na(x)] <- NA
  }  else if(class(x) == "character")  {
    res <- ifelse ( x >= rng[1] & x < rng[2], TRUE, FALSE )
  } else {
    res <- rep(NA, length(x))
  }  
  return(res)
}
