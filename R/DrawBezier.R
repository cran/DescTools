DrawBezier <-
function (x = 0, y = x, nv = 100,  col = par("col"), lty = par("lty")
  , lwd = par("lwd"), plot = TRUE ) {

    if (missing(y)) {
        y <- x[[2]]
        x <- x[[1]]
    }
    n <- length(x)
    X <- Y <- single(nv)
    Z <- seq(0, 1, length = nv)
    X[1] <- x[1]
    X[nv] <- x[n]
    Y[1] <- y[1]
    Y[nv] <- y[n]
    for (i in 2:(nv - 1)) {
        z <- Z[i]
        xz <- yz <- 0
        const <- (1 - z)^(n - 1)
        for (j in 0:(n - 1)) {
            xz <- xz + const * x[j + 1]
            yz <- yz + const * y[j + 1]
            const <- const * (n - 1 - j)/(j + 1) * z/(1 - z)
# debugging only:
#            if (is.na(const)) print(c(i, j, z))
        }
        X[i] <- xz
        Y[i] <- yz
    }
    if(plot) lines(x = as.single(X), y = as.single(Y), col=col, lty=lty, lwd=lwd )
    invisible(list(x = as.single(X), y = as.single(Y)))
}
