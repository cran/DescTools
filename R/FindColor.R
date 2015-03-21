FindColor <-
function(x, cols=rev(heat.colors(100)), min.x=min(pretty(x)), max.x=max(pretty(x)), 
                      all.inside = FALSE){

	# Korrektur von min und max, wenn nicht standardmaessig 
	colrange <- range( pretty(c(min.x,max.x)) )

	# Berechnung des entsprechenden Farb-Index
  col.idx <- findInterval(x, seq(colrange[1], colrange[2], length = length(cols) + 1)
                          , rightmost.closed=TRUE, all.inside=all.inside)
  col.idx[col.idx==0] <- NA  # den Index 0 gibt es nicht im Farbenvektor
  cols[col.idx]
  
  # alt:
	# cols[ findInterval( x, seq(colrange[1], colrange[2], length=length(cols)+1 ) ) ]
}
