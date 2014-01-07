Primes <-
function (n) {
# Source: sfsmisc
# Bill Venables (<= 2001); Martin Maechler gained another 40% speed, working with logicals and integers. 
    if ((M2 <- max(n)) <= 1) 
        return(integer(0))
    P <- rep.int(TRUE, M2)
    P[1] <- FALSE
    M <- as.integer(sqrt(M2))
    n <- as.integer(M2)
    for (p in 1:M) if (P[p]) 
        P[seq(p * p, n, p)] <- FALSE
    (1:n)[P]
}
