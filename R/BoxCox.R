BoxCox <-
function (x, lambda) {
# Author: Rob J Hyndman}
# origin: library(forecast)
    if (lambda < 0) 
        x[x < 0] <- NA
    if (lambda == 0) 
        out <- log(x)
    else out <- (sign(x) * abs(x)^lambda - 1)/lambda
    if (!is.null(colnames(x))) 
        colnames(out) <- colnames(x)
    return(out)
}
