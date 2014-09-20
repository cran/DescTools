BoxCox <-
function (x, lambda) {

# Author: Rob J Hyndman
# origin: library(forecast)
    if (lambda < 0) 
        x[x < 0] <- NA
    if (lambda == 0) 
        out <- log(x)
    else out <- (sign(x) * abs(x)^lambda - 1)/lambda
    if (!is.null(colnames(x))) 
        colnames(out) <- colnames(x)
    return(out)
    
# Greg Snow's Variant
# BoxCox <- function (x, lambda) 
# {
# ### Author: Greg Snow
# ### Source: Teaching Demos
# xx <- exp(mean(log(x)))
# if (lambda == 0) 
# return(log(x) * xx)
# res <- (x^lambda - 1)/(lambda * xx^(lambda - 1))
# return(res)
# }
    
}
