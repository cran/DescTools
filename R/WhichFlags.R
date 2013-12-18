WhichFlags <-
function(d.frm) {
  # liefert die Namen aller Flags (logische Var, int mit 2 levels) eines data.frames
  switch( class(d.frm)[1]
      , "data.frame" = { 
           ints <- names(d.frm)[ grep( pattern="integer", x=lapply( d.frm, class ) ) ] 
           c(
              ints[ unlist( lapply(d.frm[,ints], function(x) length(unique(na.omit(x))) == 2 )) ]
            , names(d.frm)[ grep( pattern="logical", x=lapply( d.frm, class ) ) ] 
           )
      }
	  , "integer" = { if( length(unique(na.omit(d.frm))) == 2 ) names(d.frm) else NULL }
	  , "logical" = { deparse(substitute(d.frm)) }
	  , NULL
  )	   
}
