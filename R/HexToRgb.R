HexToRgb <-
function(hex) {
  # converts a hexstring color to matrix with 3 red/green/blue rows
  # example: HexToRgb(c("#A52A2A","#A52A3B"))
  c2 <- do.call("cbind", lapply(hex, function(x) c(strtoi(substr(x,2,3), 16L), strtoi(substr(x,4,5), 16L), strtoi(substr(x,6,7), 16L))))
  return(c2)
}
