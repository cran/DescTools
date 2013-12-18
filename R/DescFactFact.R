DescFactFact <-
function( x, grp, rfrq="100" # , show.mutinf=FALSE
  , xname=deparse(substitute(x)), grpname=deparse(substitute(grp)) ) {
  
  catptab <- function(ptab) { 
    # just delete leading 0 in % (' .000' instead of 0.000 and ' - ' instead of NaN)
    out <- capture.output( round(ptab,3 ))
    out <- gsub(pattern=" 0\\.", replacement="  \\.", out)  
    out <- gsub(pattern="NaN", replacement=" - ", out) 
    cat( paste(out, collapse="\n"), "\n" )
  }

  stopifnot( length(x)==length(grp) ) 
  
  # Pairs summary
  n <- length(x)
  vn <- sum(complete.cases(x,grp))
  digits <- format.info(signif((n-vn)/n*100,3))[2]-2    # hier 3 signifikante Stellen für beide Angaben bestimmen

  d.frm <- data.frame(x, grp)
  # use only complete cases
  d.frm <- d.frm[complete.cases(d.frm),]
  d.frm <- data.frame( lapply( d.frm, factor ))
  colnames(d.frm) <- c(xname, grpname)  
  tab <- table( d.frm )

  cat("\nSummary: \n",
      "n pairs: ", .fmt(n), 
      ", valid: ", .fmt(vn), " (", round(vn/n*100, digits), "%)",
      ", missings: ", .fmt(n-vn), " (", round((n-vn)/n*100, digits), "%)",
      ", nrow: ", nrow(tab), 
      ", ncol: ", ncol(tab), "\n\n" 
      , sep="" ) 

  cat("Pearson's Chi-squared test:\n  "
	  , capture.output( chisq.test(tab))[5], "\n", sep="")
  cat(sprintf(
    "\nPhi-Coefficient   : %.3f\nContingency Coeff.: %.3f\nCramer's V        : %.3f\n" 
    , Phi(tab)
    , ContCoef(tab)
    , CramerV(tab)
    ) )

  # dev:  ordinale Masse?

  cat( "\n\nAbs. frequencies\n" )
  print( addmargins(tab) )

  if(unlist(strsplit(rfrq, NULL))[1] == "1"){ 
    cat( "\nRel. frequencies\n" )
    catptab(prop.table(tab))
  }
  
  if(unlist(strsplit(rfrq, NULL))[2] == "1"){ 
    cat( "\nRel. frequencies (% rows)\n" )
    catptab(prop.table(tab,1))
  }
  
  if(unlist(strsplit(rfrq, NULL))[3] == "1"){ 
    cat( "\nRel. frequencies (% columns)\n" )
    catptab(prop.table(tab,2))
  }
  
#   if(show.mutinf == TRUE){
#     cat( "\nMutual information\n" )
#     r.mi <- MutInf( x, grp )
#     attributes(r.mi)$dimnames <- attributes(tab)$dimnames
#     catptab(r.mi)
# 
#     cat( "\nRanking mutual information\n" )  
#     # ranks of mutual information
#     r.mi_r <- apply( -r.mi, 2, rank, na.last=T )
#     # nur Ränge 1:6 anzeigen
#     r.mi_r6 <- ifelse( r.mi_r < 7, r.mi_r, NA) 
#     attributes(r.mi_r6)$dimnames <- attributes(tab)$dimnames
#     catptab(r.mi_r6)
#   }
  cat( "\n")
  
  # display warning if not used levels are omitted
  if(length(setdiff(levels(x),levels(d.frm[,1])))!=0) message( gettextf("%s: \n  Level(s) %s omitted in table (no correspondance in %s)!",
    xname, paste( setdiff( levels(x), levels(d.frm[,1]) ), collapse=", "), grpname ))

  if(length(setdiff(levels(grp),levels(d.frm[,2])))!=0) message( gettextf("%s: \n  Level(s) %s omitted in table (no correspondance in %s)!",
    grpname, paste( setdiff( levels(grp), levels(d.frm[,2]) ), collapse=", "), xname ))
  cat("\n")
  
  # if( !is.null(warnings()) ){
    # wrd[["Selection"]]$TypeText( capture.output(warnings(envir = baseenv())) ) 
    # wrd[["Selection"]]$TypeParagraph()
    # ### reset warnings() 
    # assign("last.warning", NULL, envir = baseenv())
  # }
 
}
