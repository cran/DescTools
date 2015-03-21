Small <-
function (x, k = 5, unique = FALSE, na.rm = FALSE) {
  
  if (na.rm) 
    x <- na.omit(x)
  
  if (unique==TRUE) {
    ux <- unique(x)
    un <- length(ux)
    maxval <- sort(ux, partial = min(k, un))[min(k, un)]
    
    # we are using the rationale of rle here, as it turned out to be the fastest approach
    x <- sort(x[x<=maxval])
    n <- length(x)
    if (n == 0L) 
      res <- list(lengths = integer(), values = x)

    y <- x[-1L] != x[-n]
    i <- c(which(y | is.na(y)), n)
    res <- list(lengths = diff(c(0L, i)), values = x[i])

    # res <- unclass(rle(sort(x[x<=maxval])))
  }
  else {
    n <- length(x)
    res <- sort(x, partial = 1:min(k, n))[1:min(k, n)]
    #   lst <- as.vector(unlist(lapply(lst, "[", "val")))
    #   http://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
  }
  return(res)
}
