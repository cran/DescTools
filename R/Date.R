Date <-
function(year, month = NA, day = NA) {
  if(is.na(month) && is.na(day)) {
    # try to interpret year as yearmonthday yyyymmdd
    res <- as.Date(ISOdate(year %/% 10000, (year %% 10000) %/% 100, (year %% 100)))
  } else {  
    res <- as.Date(ISOdate(year, month, day))
  }  
  return(res)
}
