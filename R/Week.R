Week <-
function(x){
  
  # cast x to date, such as being able to handle POSIX-Dates automatically
  x <- as.Date(x)
  
  # The weeknumber is the number of weeks between the 
  # first thursday of the year and the thursday in the target week
  # der Donnerstag in der Zielwoche
  x.y <- as.integer(format(x,"%Y"))
  x.m <- as.integer(format(x,"%m"))
  x.d <- as.integer(format(x,"%d"))
  x.weekday <- Weekday(x)

  x.thursday <- (x - x.weekday + 4)
  # der erste Donnerstag des Jahres
  jan1.weekday <- Weekday(as.Date(paste(x.y, "01-01", sep="-")))
  first.thursday <- as.Date(paste(x.y, "01", (5 + 7*(jan1.weekday > 4) - jan1.weekday), sep="-"))

  isown <- (as.integer(x.thursday - first.thursday) %/% 7) + 1 - ((x.weekday < 4) & (Year(x.thursday) != Year(first.thursday)))*52
  isown <- ifelse(isown == 0, Week(as.Date(paste(x.y-1, "12-31", sep="-"))), isown)
  return(isown)
}
