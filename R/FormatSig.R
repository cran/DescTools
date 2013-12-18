FormatSig <-
function(x, breaks=c(0,0.001,0.01,0.05,0.1,1), labels=c("***","**","*","."," ")){
  # example: FormatSig(c(0.3, 0.08, 0.042, 0.001))
  as.character(cut(x, breaks=breaks, labels=labels, include.lowest=TRUE))
}
