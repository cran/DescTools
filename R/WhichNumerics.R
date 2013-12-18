WhichNumerics <-
function(d.frm, type=c("all","numeric","integer"), excl.flags=FALSE ) {
  # liefert die Namen aller kardinalskalierten Variablen eines data.frames
  ints <- names(d.frm)[ grep( 
              pattern=switch( match.arg(type), "all"="integer|numeric", "numeric"="numeric", "integer"="integer" )	
            , x=lapply( d.frm, class )
            ) ]
  if( !excl.flags ){
    ints} else { ints[ !ints %in% WhichFlags(d.frm) ] }
  
}
