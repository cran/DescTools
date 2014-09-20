Desc.logical <-
function(x, main=NULL, digits=3, conf.level=0.95, plotit=getOption("plotit", FALSE), ...) {

  #    if( nrow(tab) <=2 ) only 2 levels, this is a flag
  if( is.null(main)) main <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  
  cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" ) 
  if(!identical(main, NA))  cat( main )
  if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )

  # format values according to defined pretty nums
  lres <- list("length"=length(x), "n"=length(na.omit(x)), "NAs"=sum(is.na(x)), "unique"=length(na.omit(unique(x))))        
  lfmt <- lapply( lres, .fmt )        
  # what's max width in names and formatted values?
  width <- max( c( unlist(lapply(lfmt, nchar)), unlist(lapply(names(lfmt), nchar))))  

  cat( "\n\n")
  cat( paste(.txtline(lfmt, width=width, ind="  ", space=" "), collapse="\n" ), "\n")

  # think we dont need dots here...(?), this will raise an error if ... can not be interpreted by table!
  # tab <- table(x, ...)
  tab <- table(x)

  # don't display obsolete cumulative freqs if there are only two levels...
  # calculate Wilson confidence intervals instead
  ci <- t(cbind(ci <- BinomCI(x=tab[1], n=sum(tab), conf.level=conf.level)[2:3], rev(1-ci)))
  lres$BinomCI <- ci

  conf_x <- gsub("0\\.", "\\.", format(conf.level, nsmall=2))
  colnames(ci) <- gettextf(c("lci%s","uci%s"), conf_x)

  txt <- gsub(pattern=" 0\\.", replacement="  \\.", x=capture.output(
      round(rbind( cbind( freq=tab, perc=prop.table(tab), ci)), digits)) )
  
  cat( paste(txt[1], Coalesce(getOption("footnote1"),"'"), sep=""), txt[-1], sep="\n")
  cat( gettextf("\n%s %s%s-CI Wilson\n\n", Coalesce(getOption("footnote1"),"'"), 
                conf.level * 100, "%") )  
  
  if(plotit) PlotDesc.logical(x, main=main)

  invisible(lres)
  
}
