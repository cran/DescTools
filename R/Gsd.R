Gsd <-
function (x, na.rm = FALSE) {
    if (na.rm) x <- na.omit(x)
    return( exp(sd(log(x))) )
}
