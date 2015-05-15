PlotDesc.Date <-
function(x, main = deparse(substitute(x)), breaks = NULL, ..., wrd = NULL) {

  # example:
  # PlotDesc.Date( x, newwin=TRUE )
 
  # plots exp-obs dotcharts of weekdays and months
  oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) ) 
  
  # par(mar=c(10.1,3.1,4.1,1.1), oma=c(0,9,0,0), mfrow=c(1,1))
  par(oma=c(0,9,0,0))
  
  tab <- c(table( factor( format( x, "%A"), levels=format(ISOdate(2000, 1, 3:9), "%A"), ordered=TRUE) ))
  r.chi <- chisq.test(rev(tab))

  dotchart( as.numeric(r.chi$exp[]), xlim=range(pretty(range(c(r.chi$exp[],r.chi$obs[]))))
    , color="black", bg="white", pch=21, cex=0.8, xpd=TRUE  )
  mtext(side=2, at=1:7, line=2, names(r.chi$exp), las=1)
  points( x=r.chi$obs[], y=1:7, col="black", bg="black", pch=21, cex=1.2 )
  points( x=r.chi$exp[], y=1:7, col="black", bg="white", pch=21, cex=1.2 )
  
  if(!is.na(main) & is.null(wrd)) title(main=gettextf("%s (a: weekday)", main))
  
  if(!is.null(wrd)) WrdPlot(width=6.5, height=5, dfact=2.5, wrd=wrd, append.cr=TRUE)
  
  # Haeufigkeiten normiert mit Anzahl Tagen im Monat
  # par(mar=c(10.1,3.1,1.1,1.1))

  ydays <- factor( format(seq(from=as.Date("2010-01-01")
    ,to=as.Date("2010-12-31"), by="day"), "%B"), levels=format(ISOdate(2000, 1:12, 1), "%B") )
  r.chi <- chisq.test( rev(c(table(factor(format(x, "%B"),levels=levels(ydays)))))
    , p=prop.table( rev( c(table(ydays)))) )
  month_xlim <- range(pretty(range(c(r.chi$exp[],r.chi$obs[]))))  
  dotchart( as.numeric(r.chi$exp[]), xlim=month_xlim
    , color="black", bg="white", pch=21, cex=0.8, xpd=TRUE  )
  mtext(side=2, at=1:12, line=2, names(r.chi$exp), las=1)
  points( x=r.chi$obs[], y=1:12, col="black", bg="black", pch=21, cex=1.2 )
  points( x=r.chi$exp[], y=1:12, col="black", bg="white", pch=21, cex=1.2 )

  legend(x="bottom", inset=-0.5, legend=c("expected","observed"), xpd=TRUE, ncol=2
    , pch=c(21), col=c("black","black"), bg="white", pt.bg=c("white","black"), cex=1
    , pt.cex=1, xjust=0.5, adj=c(0,0.5),  text.width=c(4,4) )  
  
  if(!is.na(main) & is.null(wrd)) title(main=gettextf("%s (b: month)", main))

  if(!is.null(wrd)) WrdPlot(width=6.5, height=6.2, dfact=2.5, wrd=wrd, append.cr=TRUE)
  
  # breaks can be:  c("month","days","weeks","quarter","year")

  # old:
  #   tab <- table(cut(x, breaks))
  #   
  #   # par( mar=c(8.1,3.1,3.1,1.1) )
  #   plot( y=as.vector(tab), x=as.Date(names(tab)), type="h", cex=0.8, xlab="abs. frq.", xaxt="n"
  #     , ylab="", ... )
  #   axis.Date(1, cex.axis=0.8, at=seq(as.Date(names(tab)[1]), as.Date(rev(names(tab))[1]), breaks)
  #     , labels=strftime(seq(as.Date(names(tab)[1]), as.Date(rev(names(tab))[1]), breaks), "%y-%m") )

  if(is.null(breaks)) {
    # get some appropiate default for the breaks
    dd <- as.integer(diff(range(x, na.rm=TRUE)))
    dw <- dd / (360/52) # weeks
    dm <- dd / (360/12) # months
    dq <- dd / (360/4) # quarters
    dy <- dd / 360 # years
    
    breaks <- ifelse(dy<12, ifelse(dq<12, ifelse(dm<12, ifelse(dw<12,"days","weeks"),"months"), "quarters"), "years")
  }
  Mar(,0)
  hist(x, breaks=breaks, main=NA)
  
  if(!is.na(main) & is.null(wrd)) title(main=gettextf("%s (c: %s)", main, breaks))
  # return(invisible(c(devlst, dev.cur()))) # return window numbers ..
  
  if(!is.null(wrd)) WrdPlot(width=6.5, height=4, dfact=2.5, wrd=wrd, append.cr=TRUE)
  invisible()
  
}
