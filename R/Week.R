Week <-
function(x, method = c("iso", "us")){
  
  # cast x to date, such as being able to handle POSIX-Dates automatically
  x <- as.Date(x)
  
  method <- match.arg(method, c("iso", "us"))
  switch(method,
    "iso" = {
        
#??? fast implementation in lubridate:
      
#       xday <- ISOdate(year(x), month(x), day(x), tz = tz(x))
#       dn <- 1 + (wday(x) + 5)%%7
#       nth <- xday + ddays(4 - dn)
#       jan1 <- ISOdate(year(nth), 1, 1, tz = tz(x))
#       1 + (nth - jan1)%/%ddays(7)
      
      
      # The weeknumber is the number of weeks between the 
      # first thursday of the year and the thursday in the target week
      # der Donnerstag in der Zielwoche
#       x.y <- Year(x)
#       x.weekday <- Weekday(x)
#         
#       x.thursday <- (x - x.weekday + 4)
#       # der erste Donnerstag des Jahres
#       jan1.weekday <- Weekday(as.Date(paste(x.y, "01-01", sep="-")))
#       first.thursday <- as.Date(paste(x.y, "01", (5 + 7*(jan1.weekday > 4) - jan1.weekday), sep="-"))
#       
#       wn <- (as.integer(x.thursday - first.thursday) %/% 7) + 1 - ((x.weekday < 4) & (Year(x.thursday) != Year(first.thursday)))*52
#       wn <- ifelse(wn == 0, Week(as.Date(paste(x.y-1, "12-31", sep="-"))), wn)

      z <- x + (3 - (as.POSIXlt(x)$wday + 6) %% 7)
      # jan1 <- as.Date(gettextf("%s-01-01", Year(z)))
      jan1 <- as.Date(paste(Year(z), "-01-01", sep=""))
      
      wn <- 1 + as.integer(z - jan1) %/% 7
      
    },
    "us"={
      wn <- as.numeric(strftime(as.POSIXlt(x), format="%W"))
    }
  )
  return(wn)
  
}
