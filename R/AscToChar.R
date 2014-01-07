AscToChar <-
function(i) { 
# old version:
# example: AscToChar(x.char)
#  ASCII <- intToUtf8(1:256, multiple=TRUE)
#  ASCII[128:159] <- c("€","","‚","ƒ","„","…","†","‡","ˆ","‰","Š","‹","Œ","","Ž","","","‘","’","“","”","•","–","—","˜","™","š","›","œ","","ž","Ÿ")
#  ASCII[i%%256 + 1] 

  # new and far more elegant
  # ref: http://datadebrief.blogspot.ch/search/label/R
  rawToChar(as.raw(i))
}
