GetPairs <-
function(x, y = NULL) {
  # liefert einen data.frame mit allen paarweisen Kombinationen der Variablen
  if( missing(y)) {  # kein y vorhanden, use x only
    data.frame( t(combn(x, 2)), stringsAsFactors=F )
  } else {  
    # wenn y definiert ist, wird all.x zu all.y zurückgegeben
    expand.grid(x, y, stringsAsFactors=F )
  }  
}
