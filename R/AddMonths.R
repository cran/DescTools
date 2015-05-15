AddMonths <-
function (x, n, ceiling = TRUE) {

  if (IsDate(x)) {
    # ref: http://stackoverflow.com/questions/14169620/add-a-month-to-a-date
    # Author: Antonio
    
    # no ceiling
    res <- sapply(x, seq, by = paste(n, "months"), length = 2)[2,]
    # sapply kills the Date class, so recreate here
    class(res) <- class(x)
    
    #ceiling    
    DescTools::Day(x) <- 1
    res_c <- sapply(x, seq, by = paste(n + 1, "months"), length = 2)[2,] - 1
    class(res_c) <- class(x)
    
    #use ceiling in case of overlapping
    res <- pmin(res, res_c)
  }
  else if (all(DescTools::IsWhole(x))) {
    res <- sapply(x, function(i){
      if (i %[]% c(100001, 999912)) {
        # Author: Roland Rapold
        # YYYYMM
        y <- i%/%100
        m <- i - y * 100
        res <- (y - 10 + ((m + n + 120 - 1)%/%12)) * 100 + 
          ((m + n + 120 - 1)%%12) + 1
      }
      else if (i %[]% c(10000101, 99991231)) {
        # YYYYMMDD
        res <- DescTools::AddMonths(x = as.Date(as.character(i), "%Y%m%d"), n = n, ceiling = ceiling)
        res <- DescTools::Year(res)*10000 + DescTools::Month(res)*100 + Day(res)
      }
      return(res)
    })
  }
  else {
    res <- NA
  }
  return(res)
  
}
