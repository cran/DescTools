Desc.factor <-
function(x, xname = NULL, ord=c("desc","asc","name","level"), 	
  maxrows = 12, 		# how many levels should be displayed, set NA if all wanted
  digits = 3, plotit = FALSE, ...) {
  
  # example:
  #   data(d.pizza, package="DescTools")
  #   Desc.factor(d.pizza$driver)
  
  if( is.null(xname)) xname <- gettextf("%s (%s)", deparse(substitute(x)), paste(class(x), collapse=", "))
  
  # if only 2 levels it's a flag, use Desc.logical instead
  if( nlevels(x)==2 ) { 
    Desc.logical(x, xname=gsub(pattern="factor)", replacement="factor - dichotomous)", x=xname)) 
    return(invisible())
  } 

  cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" ) 
  if(!is.na(xname))  cat( xname )
  if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )
  
  # format values according to defined pretty nums
  lfmt <- list("length"=length(x), "n"=length(na.omit(x)), "NAs"=sum(is.na(x)) )
  if(inherits(x, "factor")) lfmt$levels <- nlevels(x)  
  lfmt$unique <- length(na.omit(unique(x)))
  lfmt$dupes <- lfmt$unique != lfmt$length
  
  lres <- lfmt
  lfmt <- lapply( lfmt, .fmt )  
  lfmt$dupes <- ifelse(lfmt$dupes=="0", "n", "y")
  
  # what's max width in names and formatted values?
  width <- max( c( unlist(lapply(lfmt, nchar)), unlist(lapply(names(lfmt), nchar))))  

  cat( "\n\n")
  cat( paste(.txtline(lfmt, width=width, ind="  ", space=" "), collapse="\n" ), "\n")

  # calculate the frequencies
  lres$frq <- Freq( x=x 
             , ord=match.arg( arg=ord, choices=c("desc","asc","name","level")) 
             , digits=digits, print = FALSE)      
  
  if(is.na(maxrows)) { maxrows <- nrow(lres$frq) }
  
  if(maxrows < 1) { maxrows <- sum(lres$frq[,5] < maxrows) + 1 }
# restrict table to maxrows  
  lres$frq <- lres$frq[1:min(nrow(lres$frq), maxrows),]

  txt.frq <- capture.output(lres$frq)
  txt.frq <- paste(substr(txt.frq, 0, regexpr("level", txt.frq[1]) + 5), 
                   gsub(pattern="0\\.", replacement=" \\.", 
                        x=substr(txt.frq, regexpr("level", txt.frq[1])+5+1, nchar(txt.frq[1]))), 
                   sep="")

  cat("\n")
#  cat(txt.frq[1:min(length(txt.frq), maxrows+1)], sep="\n")
  cat(txt.frq, sep="\n")
  if( maxrows+1 <length(txt.frq) ) cat("... etc.\n [list output truncated]\n\n")  else cat("\n")

  if(plotit) PlotDesc.factor(x, main=xname, maxrows = maxrows)
  
  invisible(lres)
  
}
