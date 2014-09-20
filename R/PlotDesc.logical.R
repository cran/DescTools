PlotDesc.logical <-
function(x, main = deparse(substitute(x)), xlab="", 
                             col1=getOption("col1", hblue), col2=getOption("col2", hred), ..., wrd=NULL) {

  tab <- table(factor(x))
  if(nrow(tab)>2) stop( "!PlotDesc.logical! can only display 2 levels" )
  ptab <- prop.table(tab)

  oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) ) 
  # usr <- par("usr"); on.exit(par(usr)) 

  par(mar=c(4.1,2.1,0,2.1))
  if(!is.na(main)) par(oma=c(0,0,3,0))

  plot( x=ptab[1], y=1, cex=0.8, xlim=c(0,1), yaxt="n", ylab="", type="n", bty="n", xlab=xlab, main=NA)
  segments( x0=0, x1=1, y0=1, y1=1, col="grey")
  segments( x0=c(0,1), x1=c(0,1), y0=0.8, y1=1.2, col="grey")
  # insert grid
  segments( x0=seq(0,1,0.1), x1=seq(0,1,0.1), y0=0.8, y1=1.2, col="grey", lty="dotted")
  rect(xleft=0, ybottom=0.95, xright=ptab[1], ytop=1.05, col=col1 )     # greenyellow
  rect(xleft=ptab[1], ybottom=0.95, xright=1, ytop=1.05, col=col2 )     # green4
  ci.99 <- BinomCI(tab[1], sum(tab), conf.level=0.99)[2:3]
  ci.95 <- BinomCI(tab[1], sum(tab), conf.level=0.95)[2:3]
  ci.90 <- BinomCI(tab[1], sum(tab), conf.level=0.90)[2:3]
  rect(xleft=ci.99[1], ybottom=0.9, xright=ci.99[2], ytop=1.1, col="grey80" ) # olivedrab1
  rect(xleft=ci.95[1], ybottom=0.9, xright=ci.95[2], ytop=1.1, col="grey60" ) # olivedrab3
  rect(xleft=ci.90[1], ybottom=0.9, xright=ci.90[2], ytop=1.1, col="grey40" ) # olivedrab4
  segments( x0=ptab[1], x1=ptab[1], y0=0.7, y1=1.3)
  
  legend( x=0, y=0.75, legend=c("ci.99     ","ci.95     ","ci.90     "), box.col="white"
    , fill=c("grey80","grey60","grey40"), bg="white", cex=1, ncol=3, text.width=c(0.2,0.2,0.2) )
  text( names(ptab), x=c(ptab[1], ptab[1] + 1)/2, y=1.2 )
  if(!is.na(main)) title(main=main, outer=TRUE)
  
  if(!is.null(wrd)) WrdPlot(width=6, height=4, dfact=2.6, crop=c(0.2,0.2,2,0), wrd=wrd, append.cr=FALSE)
  invisible()
  
}
