Interval <-
function(xp, yp){
  # calculates the number of days of the overlapping part of two date periods
  length(intersect(xp[1]:xp[2], yp[1]:yp[2]))
}
