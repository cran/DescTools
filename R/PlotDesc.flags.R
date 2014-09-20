PlotDesc.flags <-
function(x, ..., wrd = NULL){
  
  oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) ) 
  
  flags <- do.call(rbind, lapply(x, function(z) {
    tab <- table(z)
    BinomCI(tab[1], sum(tab))
  }))
  rownames(flags) <- names(x)
  
  par(mai=c(1, max(strwidth(rev(rownames(flags)), "inch"))+2, 0.2, .3)+.02) 
  PlotDotCI(flags, lwd = 20, code = 0, cex=0.9,
            lcol = PalHelsana()["hellgruen"], pch="|", pch.cex = 2.5, xlim=c(0,1), lend="butt")
  
  if(!is.null(wrd)) WrdPlot(width=8, height=pmin(3+3/6*nrow(flags), 10), 
                            dfact=2.5, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
  invisible()
  
}
