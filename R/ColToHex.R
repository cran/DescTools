ColToHex <-
function(col, alpha=1) {
  col.rgb <- col2rgb(col)
  col <- apply( col.rgb, 2, function(x) sprintf("#%02X%02X%02X", x[1], x[2], x[3]) )
  if(alpha != 1 ) col <- paste( col, DecToHex( round( alpha * 255, 0)), sep="")
  return( col )
  # old: sprintf("#%02X%02X%02X", col.rgb[1], col.rgb[2], col.rgb[3])
}
