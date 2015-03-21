SiegelTukeyRank <-
function(x, g, drop.median = TRUE) {
    
    # they do not drop the median in:
    # http://en.wikipedia.org/wiki/Siegel%E2%80%93Tukey_test
    # A <- c(33,62,84,85,88,93,97); B <- c(4,16,48,51,66,98)
    # this is wrong there, as the author did not leave the median out
  
    ord.x <- order(x, g)
    sort.x <- x[ord.x]
    sort.id <- g[ord.x]
    
    n <- length(x)
    if(drop.median){
      if(n %% 2 > 0) {
        # gonna have to drop the (first) median value
        # as we sorted by the groupsize, this will be the one out of the bigger group (if existing)
        fm <- which( sort.x == median(sort.x))[1]
        sort.x <- sort.x[-fm]
        sort.id <- sort.id[-fm]
        n <- n-1
      }
    }
    
    base1 <- c(1, 4)
    iterator1 <- matrix(seq(from = 1, to = n, by = 4)) - 1
    rank1 <- apply(iterator1, 1, function(x) x + base1)
    
    iterator2 <- matrix(seq(from = 2, to = n, by = 4))
    base2 <- c(0, 1)
    rank2 <- apply(iterator2, 1, function(x) x + base2)
    
    if (length(rank1) == length(rank2)) {
        rank <- c(rank1[1:floor(n/2)], rev(rank2[1:ceiling(n/2)]))
    } else {
        rank <- c(rank1[1:ceiling(n/2)], rev(rank2[1:floor(n/2)]))
    }
    
    unique.ranks <- tapply(rank, sort.x, mean)
    unique.x <- as.numeric(as.character(names(unique.ranks)))
    
    ST.matrix <- merge( 
       data.frame(sort.x, sort.id),          # this are the original values in x-order
       data.frame(unique.x, unique.ranks),   # this is the rank.matrix 
       by.x = "sort.x", by.y = "unique.x")
    
    return(ST.matrix)
}
