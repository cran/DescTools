AddMonths <-
function (x, n, ceiling = TRUE) {
  
  if (IsDate(x)) {
    # ref: http://stackoverflow.com/questions/14169620/add-a-month-to-a-date
    # Author: Antonio
    
    #no ceiling
    res <- seq(x, by = paste(n, "months"), length = 2)[2]

    #ceiling    
    Day(x) <- 1
    res_c <- seq(x, by = paste(n + 1, "months"), length = 2)[2] - 
      1
    #use ceiling in case of overlapping
    if (res_c < res) res <- res_c
  }
  else if (IsWhole(x)) {
    if(x %[]% c(100001,999912)){
      # Author: Roland Rapold
      # YYYYMM
      y <- x %/% 100
      m <- x - y * 100
      res <- (y - 10 + ((m + n + 120 - 1) %/% 12)) * 100 + ((m + n + 120 - 1) %% 12) + 1
      
    } else if(x %[]% c(10000101, 99991231)){
      # YYYYMMDD
      res <- AddMonths(x=as.Date(as.character(x), "%Y%m%d"), n = n, ceiling = ceiling)
    }  
  }
  else {
    res <- NA
  }
  return(res)
}
