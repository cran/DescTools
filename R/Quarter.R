Quarter <-
function (x) { 
  # Berechnet das Quartal eines Datums
  # y <- as.numeric( format( x, "%Y") )
  # paste(y, "Q", (as.POSIXlt(x)$mon)%/%3 + 1, sep = "") 
  # old definition is counterintuitive...
  return((as.POSIXlt(x)$mon)%/%3 + 1)
}
