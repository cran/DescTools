Factorize <-
function (n) {
  # Factorize <- function (n, verbose = FALSE) {
  # Source sfsmisc: Martin Maechler, Jan. 1996. 
    if (all(n < .Machine$integer.max)) 
        n <- as.integer(n)
    else {
        warning("factorizing large int ( > maximal integer )")
        n <- round(n)
    }
    N <- length(n)
    M <- as.integer(sqrt(max(n)))
    k <- length(pr <- Primes(M))
    nDp <- outer(pr, n, FUN = function(p, n) n%%p == 0)
    res <- vector("list", length = N)
    names(res) <- n
    for (i in 1:N) {
        nn <- n[i]
        if (any(Dp <- nDp[, i])) {
            nP <- length(pfac <- pr[Dp])
#            if (verbose) cat(nn, " ")
        }
        else {
            res[[i]] <- cbind(p = nn, m = 1)
#            if (verbose) cat("direct prime", nn, "\n")
            next
        }
        m.pr <- rep(1, nP)
        Ppf <- prod(pfac)
        while (1 < (nn <- nn%/%Ppf)) {
            Dp <- nn%%pfac == 0
            if (any(Dp)) {
                m.pr[Dp] <- m.pr[Dp] + 1
                Ppf <- prod(pfac[Dp])
            }
            else {
                pfac <- c(pfac, nn)
                m.pr <- c(m.pr, 1)
                break
            }
        }
        res[[i]] <- cbind(p = pfac, m = m.pr)
    }
    res
}
