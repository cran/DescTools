CatTable <-
function( tab, wcol, nrepchars, width=getOption("width") ) {

  # Wie viele Datenspalten haben vollständig Platz auf einer Linie?
  ncols <- ( width - nrepchars ) %/% wcol
  # Wieviele Zeilen ergeben sich?
  nrows <- ((nchar(tab[1]) - nrepchars) %/% wcol) / ncols + 
    (((nchar(tab[1]) - nrepchars) %% wcol ) > 0) *1  # Rest Linie
  for( i in 1:nrows ) {
    for( j in 1:length(tab) ){
  #    cat( i, nrepchars + 1 + (i-1)*(ncols*wcol-4), nrepchars + i*ncols*wcol-5, "\n")
      cat( substr(tab[j],1,nrepchars)
	       , substr(tab[j], nrepchars + 1 + (i-1)*(ncols*wcol), nrepchars + 1 + i*ncols*wcol-1 )
	       , "\n", sep="" )
    } 		 
	cat( "\n" )
	}
}
