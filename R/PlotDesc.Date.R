PlotDesc.Date <-
function(x, breaks="month", ..., wrd=NULL) {

  # example:
  # PlotDesc.Date( x, newwin=TRUE )
 
  # plots exp-obs dotcharts of weekdays and months
  oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) ) 
  
  par(mar=c(10.1,3.1,4.1,1.1), oma=c(0,9,0,0), mfrow=c(1,1))

  tab <- table( factor( format( x, "%A"), levels=format(ISOdate(2000, 1, 3:9), "%A"), ordered=TRUE) )
  r.chi <- chisq.test(rev(tab))

  dotchart( as.numeric(r.chi$exp[]), xlim=range(pretty(range(c(r.chi$exp[],r.chi$obs[]))))
    , color="black", bg="white", pch=21, cex=0.8, xpd=TRUE  )
  mtext(side=2, at=1:7, line=2, names(r.chi$exp), las=1)
  points( x=r.chi$obs[], y=1:7, col="black", bg="black", pch=21, cex=1.2 )
  points( x=r.chi$exp[], y=1:7, col="black", bg="white", pch=21, cex=1.2 )
  
  if(!is.null(wrd)) WrdPlot(width=6.5, height=5, dfact=2.5, wrd=wrd, append.cr=TRUE)
  
  # Häufigkeiten normiert mit Anzahl Tagen im Monat
  par(mar=c(10.1,3.1,1.1,1.1))

  ydays <- factor( format(seq(from=as.Date("2010-01-01")
    ,to=as.Date("2010-12-31"), by="day"), "%B"), levels=format(ISOdate(2000, 1:12, 1), "%B") )
  r.chi <- chisq.test( rev(table(factor(format(x, "%B"),levels=levels(ydays))))
    , p=prop.table( rev( table(ydays))) )
  month_xlim <- range(pretty(range(c(r.chi$exp[],r.chi$obs[]))))  
  dotchart( as.numeric(r.chi$exp[]), xlim=month_xlim
    , color="black", bg="white", pch=21, cex=0.8, xpd=TRUE  )
  mtext(side=2, at=1:12, line=2, names(r.chi$exp), las=1)
  points( x=r.chi$obs[], y=1:12, col="black", bg="black", pch=21, cex=1.2 )
  points( x=r.chi$exp[], y=1:12, col="black", bg="white", pch=21, cex=1.2 )

  legend(x="bottom", inset=-0.5, legend=c("expected","observed"), xpd=TRUE, ncol=2
    , pch=c(21), col=c("black","black"), bg="white", pt.bg=c("white","black"), cex=1
    , pt.cex=1, xjust=0.5, adj=c(0,0.5),  text.width=c(4,4) )  
  
  if(!is.null(wrd)) WrdPlot(width=6.5, height=6.2, dfact=2.5, wrd=wrd, append.cr=TRUE)
  
  # breaks can be:  c("month","days","weeks","quarter","year")
  tab <- table(cut(x, breaks))
  
  par( mar=c(8.1,3.1,3.1,1.1) )
  plot( y=as.vector(tab), x=as.Date(names(tab)), type="h", cex=0.8, xlab="abs. frq.", xaxt="n"
    , ylab="", ... )
  axis.Date(1, cex.axis=0.8, at=seq(as.Date(names(tab)[1]), as.Date(rev(names(tab))[1]), breaks)
    , labels=strftime(seq(as.Date(names(tab)[1]), as.Date(rev(names(tab))[1]), breaks), "%y-%m") )
  
  # return(invisible(c(devlst, dev.cur()))) # return window numbers ..
  
  if(!is.null(wrd)) WrdPlot(width=6.5, height=4, dfact=2.5, wrd=wrd, append.cr=TRUE)
  
}
