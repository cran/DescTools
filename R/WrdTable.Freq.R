WrdTable.Freq <-
function(tab, wrd = getOption("lastWord"), row.names = FALSE, ...){
  
  tab[,c(3,5)] <- sapply(round(tab[,c(3,5)], 3), FormatFix, after=3)
  WrdTable.default(tab=tab, wrd=wrd)
  
  invisible()
  
}
