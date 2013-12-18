`%[)%` <-
function(x, rng) { 
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
