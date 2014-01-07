DescWrd.data.frame <-
function(x, wrd, caption=deparse(substitute(x)) 
  , univar=TRUE, colpairs=FALSE, notch=TRUE, col0="#9A0941FF", col1="#8296C4FF"
  , width=100, fontname="Consolas", fontsize=7, ...) {
 
  # 20.5.2010: Keine Warnungen innerhalb der Funktion möglich!
  #            warnings() wird erst nach Abschluss der Funktion befüllt.
   
  d.frm <- x  
  
  # Start report:     data.frame  Infos einfügen **************
  WrdCaption( "Describe data.frame", wrd=wrd )  
  wrd[["Selection"]]$TypeParagraph()
  WrdText( capture.output( Str(d.frm) ), wrd=wrd )
  wrd[["Selection"]]$TypeParagraph()
  wrd[["Selection"]]$TypeParagraph()
  
  # debug:    cx <- colnames(d.frm)[1]
  # die univariaten Darstellungen (nur wenn erwünscht) ******************
  if( univar == TRUE ) {
    for( cx in colnames(d.frm) ){
      DescWrd.default( x=d.frm[,cx], 
        # caption example:   1 : zustellzeit_h (numeric)
        caption=paste(match(cx,colnames(d.frm)), " : ", gsub(" $","", gsub("^ +", "", cx))
            , " (", paste(class(d.frm[,cx]),collapse=" "), ")", sep="" ), 
        wrd=wrd )
    }
  }
  
  # exit function if no pairwise analysis required
  if(colpairs==FALSE) {  return( invisible(wrd) )  }

  # bei paarweisen Darstellungen sollen flags behandelt werden wie factors
  for( z in WhichFlags(d.frm) ) d.frm[,z] <- factor(d.frm[,z]) 

  # die paarweisen Darstellungen  ************************
  # 1.1) factor ~ factor pairwise        debug: i <- 1
  if( length(WhichFactors(d.frm)) > 1 ){
    vpairs <- GetPairs(WhichFactors(d.frm))
    if(nrow(vpairs) > 0) {
      for( i in 1:nrow(vpairs) ) {
        cx <- vpairs[i,1]; cy <- vpairs[i,2]

        # Titel am Anfang
        WrdCaption( paste( match(cx,colnames(d.frm)), " ~ ", match(cy,colnames(d.frm))
          , " : ", cx, " ~ ", cy, sep="" ), wrd=wrd)
        WrdText( capture.output( DescFactFact( x=d.frm[,cx], 
            grp=d.frm[,cy], xname=cx, grpname=cy )), wrd=wrd)

        # 2 mosaicplots for factor ~ factor representation
# old:        PlotDescFactFact( table(d.frm[,c(cx,cy)]), newwin=TRUE, col0=col0, col1=col1 )
        PlotDesc(table(d.frm[,c(cx,cy)]), col0=col0, col1=col1, wrd=wrd)
      }
    }  
  }
  
  # 1.2) numeric ~ factor pairwise
  if( length(WhichFactors(d.frm)) > 0 & length(WhichNumerics(d.frm)) > 0 ){
    vpairs <- GetPairs(WhichNumerics(d.frm), WhichFactors(d.frm))
    if(nrow(vpairs) > 0) {
      for( i in 1:nrow(vpairs) ) {
        cx <- vpairs[i,1]; cy <- vpairs[i,2]
        
        # Titel am Anfang
        WrdCaption( paste( match(cx,colnames(d.frm)), " ~ ", match(cy,colnames(d.frm))
          , " : ", cx, " ~ ", cy, sep="" ), wrd=wrd)
        WrdText( capture.output( DescNumFact( d.frm[,cx], d.frm[,cy]
          , width=width, digits=c(2,2,2,2,0,3,0,0) ) ), wrd=wrd)

        # Boxplot für numeric ~ factor Darstellung
        PlotDescNumFact( formula=formula(paste(cx, "~", cy)), data=d.frm[,c(cx,cy)], 
                         notch=notch, wrd=wrd, main="")
      }
    }
  }
  
  # 1.2) numeric ~ numeric pairwise
  if( length(WhichNumerics(d.frm)) > 1 ){
    vpairs <- GetPairs(WhichNumerics(d.frm))
    if(nrow(vpairs) > 0) {
#      windows(width=4, height=4) ### dimension for a scatterplot
      for( i in 1:nrow(vpairs) ) {
        cx <- vpairs[i,1]; cy <- vpairs[i,2]
        
        # Titel am Anfang
        WrdCaption( paste( match(cx,colnames(d.frm)), " ~ ", match(cy,colnames(d.frm))
          , " : ", cx, " ~ ", cy, sep="" ), wrd=wrd)
        WrdText( capture.output( DescNumNum( d.frm[,unlist(cx)], d.frm[,unlist(cy)] )), wrd=wrd)

        # Scatterplot für numeric ~ numeric Darstellung
        PlotDescNumNum( form1=formula(paste(cx, "~", cy))
          , form2=formula(paste(cy, "~", cx)), data=d.frm[,c(cx,cy)], wrd=wrd, main="")
      }
    }
  }
  
  # funktioniert nicht!
  # wrd[["Selection"]]$WholeStory()
  # wrd[["Selection"]]$Fields()$Update()

   return( invisible(wrd) )
  
}
