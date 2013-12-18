DescNumFact <-
function( x, grp, digits = NULL, width=getOption("width")
  , use.na = c("no", "ifany", "always") ) {

  if( is.null(digits) ) { 
    digits <- c(NA,NA,NA,NA,0,3,0,0) 
  } else if (length(digits) == 1)  {
    digits <- c(rep(digits, 4),0,3,0,0) 
  }
  # else take them, as they are defined
  
	outline <- function(x, width, digits=NA, markext=TRUE) {
  
		out <- paste(paste( formatC( x, width=width, digits=digits, format="f" ), collapse=" "),"")
		if(markext==TRUE) {
		  for( i in which(x==min(x))*(width+1) ) substr(out,i,i) <- Coalesce(getOption("footnote1"),"'")
		  for( i in which(x==max(x))*(width+1) ) substr(out,i,i) <- Coalesce(getOption("footnote2"),'"')
		} 
		return(out)
	}
  
  # AscToChar(185), AscToChar(178)  "¹", "²"

  # Pairs summary
  n <- length(x)
  idcomp <- complete.cases(x, grp)
  vn <- sum(idcomp)
  dig <- format.info(signif((n-vn)/n*100,3))[2]-2    # hier 3 signifikante Stellen für beide Angaben bestimmen
  
  d.res <- data.frame(
      mean= tapply( x, grp, FUN=mean, na.rm=T ) 
	  , median= tapply( x, grp, FUN=median, na.rm=T )
	  , sd= tapply( x, grp, FUN=sd, na.rm=T )
	  , IQR= tapply( x, grp, FUN=IQR, na.rm=T )
#	  , n= tapply( x, grp, FUN=length )  function(x) sum(!is.na(x))
#	  , np= tapply( x, grp, FUN=length )/sum(!is.na(x))
    , n= tapply( x, grp, FUN=function(x) sum(!is.na(x)) )
	  , np= tapply( x, grp, FUN=function(x) sum(!is.na(x))) / vn
	  , NAs= tapply( x, grp, FUN=function(x) sum(is.na(x)))
	  , "0s"= tapply( x, grp, FUN=function(x) sum(na.omit(x)==0))
    , row.names=NULL
	)
  if(is.null(levels(grp))) cname <- levels(factor(grp)) else cname <- levels(grp)
  cname[is.na(cname)] <- "NA"
  rownames(d.res) <- cname
  rname <- c("mean","median","sd","IQR","n","np","NAs","0s")  # cannot use names as 0s is replaced by X.0s....

  cat("\nSummary: \n",
      "n pairs: ", .fmt(n), 
      ", valid: ", .fmt(vn), " (", round(vn/n*100, dig), "%)",
      ", missings: ", .fmt(n-vn), " (", round((n-vn)/n*100, dig), "%)",
      ", groups: ", length(cname), 
      "\n\n"
      , sep="" ) 
   
	d.fmt <- data.frame(digits=rep(NA,ncol(d.res)), width=NA)
	for( i in 1:ncol(d.res) ){
     if(is.na(digits[i])) { 
	     d.fmt$digits[i] <- Ndec(format(d.res[,i])[1])
	   } else { 
       d.fmt$digits[i] <- digits[i] 
	   }     	
     d.fmt$width[i] <- max( nchar(formatC(d.res[,i], format="f", digits=d.fmt$digits[i])) )
	}
  # d.fmt$digits[6] <- signif( d.res$np, 3)
  # d.fmt$width[6] <- max( nchar(formatC(d.res[,6], format="f", digits=d.fmt$digits[6])) )
  
  wmax <- max(c( max(d.fmt$width)   # die maximale Breite der Zahlen
    , nchar(rownames(d.res))) )     # die maximale Breite der Überschriften
  lenrowname <- max(nchar(rname))	
  
	out <- vector(mode="character", length=ncol(d.res)+1)
	out[1] <- paste( paste(rep(" ",lenrowname),collapse=""), 
	    outline(rownames(d.res), width=wmax+1, digits=NA, markext=FALSE)
		, sep=" ")

	markext <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
  cat( "\n" )
  
	for( i in 1:ncol(d.res) ){
	  out[i+1] <- paste(paste( format( rname[i], width=lenrowname )
	    , outline(d.res[,i], width=wmax+1, digits=d.fmt$digits[i], markext=markext[i]), collapse="" )
		, sep=" " )
	}
  #cat( wmax+2, "\n") 
	CatTable(out, wcol=wmax+2, nrepchars=8, width )
  cat(gettextf("%s min, %s max\n", Coalesce(getOption("footnote1"),"'"), Coalesce(getOption("footnote2"),'"'))) 
  cat("\nKruskal-Wallis rank sum test:\n  "
	  , capture.output( kruskal.test( x ~ grp, na.action = "na.omit" ))[5], "\n", sep="") 

  if((sum(is.na(grp)) > 0) & (length(grep("NA",cname))==0))
    cat(gettextf("\nWarning:\n  Grouping variable contains %s NAs (%s"
      , sum(is.na(grp)), signif(sum(is.na(grp))/length(grp), digits=3)*100), "%).\n", sep="")

  cat( "\n")
}
