BoxCoxInv <-
function(x, lambda){
    if (lambda < 0) 
        x[x > -1/lambda] <- NA
    if (lambda == 0) 
        out <- exp(x)
    else {
        xx <- x * lambda + 1
        out <- sign(xx) * abs(xx)^(1/lambda)
    }
    if (!is.null(colnames(x))) 
        colnames(out) <- colnames(x)
    return(out)
}
