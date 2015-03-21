AscToChar <-
function(i) { 
# old version:
# example: AscToChar(x.char)
#  ASCII <- intToUtf8(1:256, multiple=TRUE)

  # new and far more elegant
  # ref: http://datadebrief.blogspot.ch/search/label/R
  rawToChar(as.raw(i))
  
}
