PageTest.default <-
function (y, groups, blocks, ...) {

  p.page <- function(k, n, L){
    
    qvec <- .PageDF[k][[1]]
    f1 <- qvec
    
    for (i in 1:(n-1)) { 
      erg <- convolve(f1, qvec, conj = TRUE, type = "open")
      f1 <- erg  
    }
    p <- cumsum(erg)[n * k * (k+1) * (2*k+1)/6 + 1 - L] 
    return(p)
    
  }
  
  
  DNAME <- deparse(substitute(y))
  if (is.matrix(y)) {
    groups <- factor(c(col(y)))
    blocks <- factor(c(row(y)))
  }
  else {
    if (any(is.na(groups)) || any(is.na(blocks))) 
      stop("NA's are not allowed in 'groups' or 'blocks'")
    if (any(diff(c(length(y), length(groups), length(blocks))) != 
              0L)) 
      stop("'y', 'groups' and 'blocks' must have the same length")
    DNAME <- paste(DNAME, ", ", deparse(substitute(groups)), 
                   " and ", deparse(substitute(blocks)), sep = "")
    if (any(table(groups, blocks) != 1)) 
      stop("not an unreplicated complete block design")
    groups <- factor(groups)
    blocks <- factor(blocks)
    o <- order(groups, blocks)
    y <- y[o]
    groups <- groups[o]
    blocks <- blocks[o]
  }
  k <- nlevels(groups)
  y <- matrix(unlist(split(y, blocks)), ncol = k, byrow = TRUE)
  y <- y[complete.cases(y), ]
  n <- nrow(y)
  
  
  rnksum <- apply(apply(y, 1, rank), 1, sum)
  L <- sum(seq_along(rnksum) * rnksum)
  nc <- ncol(y)
  nr <- nrow(y)
  
  if(nc < 16){
    pval <- p.page(k=nc, n=nr, L=L)
  } else {
    mu <- nr * nc * (nc + 1)^2/4
    # sig <- nr * nc^2 * (nc + 1)^2 * (nc - 1)/144
    sigma <- nr * nc^2 * (nc+1) * (nc^2-1) / 144
    z <- (L - mu)/sqrt(sigma)
    pval <- pnorm(z, lower.tail = FALSE)
    
  }

  structure(list(statistic = c(L = L), p.value = pval, method = "Page test for ordered alternatives", 
                 data.name = DNAME), 
          class = "htest")    
}
