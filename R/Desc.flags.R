Desc.flags <-
function(x, i=1, plotit=getOption("plotit", FALSE), ...){
  
  cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" ) 
  cat( "Multiple dichotomous variables" )
  if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )
  cat("\n")
  
  cat( "\nSummary: \n", "total n: ", nrow(x), "\n\n", sep="" ) 
  
  d.sub <- x
  
  flags <- do.call(rbind, lapply(d.sub, function(z) {
    tab <- table(z)
    data.frame(val = names(tab[i]), abs=tab[i], BinomCI(tab[i], sum(tab)))
  }
  ))
  out <- data.frame( do.call(rbind,  lapply(d.sub, function(x) cbind(NAs=sum(is.na(x)), n=length(x)- sum(is.na(x)))))
                     , flags)
  out[,5:7] <- apply(out[,5:7],2, FormatFix, after=3)
  
  print(out, quote=FALSE)
  cat("\n")  
  
  if(plotit) PlotDesc(x)
  
}
