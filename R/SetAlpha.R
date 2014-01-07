SetAlpha <-
function(col, alpha=0.5) {
  
  if (length(alpha) < length(col)) alpha <- rep(alpha, length.out = length(col))
  if (length(col) < length(alpha)) col <- rep(col, length.out = length(alpha))
  
  acol <- substr(ColToHex(col), 1, 7)
  acol[!is.na(alpha)] <- paste(acol[!is.na(alpha)], DecToHex(round(alpha[!is.na(alpha)]*255,0)), sep="")
  acol[is.na(col)] <- NA
  return(acol)  
}
