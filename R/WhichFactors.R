WhichFactors <-
function(d.frm) { 
  # liefert die Namen aller Faktoren eines data.frames
  names(d.frm)[ grep( pattern="factor", x=lapply( d.frm, class ) ) ] 
}
