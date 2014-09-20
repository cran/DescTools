Gmean <-
function (x, na.rm = FALSE) {
    if (na.rm) x <- na.omit(x)
    if(any(x < 0)) return(NA)
    return( exp(mean(log(x))) )
}
