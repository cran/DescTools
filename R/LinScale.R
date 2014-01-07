LinScale <-
function (x, low = NULL, high = NULL, newlow = 0, newhigh = 1)  {

    x <- as.matrix(x)

    if(is.null(low)) { 
      low <- apply(x, 2, min, na.rm=TRUE)
    } else {
      low <- rep(low, length.out=ncol(x))
    }
    if(is.null(high)) { 
      high <- apply(x, 2, max, na.rm=TRUE)
    } else {
      high <- rep(high, length.out=ncol(x))
    }
    # do the recycling job
    newlow <- rep(newlow, length.out=ncol(x))
    newhigh <- rep(newhigh, length.out=ncol(x))
    
    xcntr <- (low * newhigh - high * newlow) / (newhigh - newlow)
    xscale <- (high - low) / (newhigh - newlow)
    
    return( scale(x, center = xcntr, scale = xscale)) 
    
}
