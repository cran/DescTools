PlotLinesA <-
function(x, y, col=1:5, lty=1, lwd=1, lend = par("lend"), xlab = NULL, 
                       ylab = NULL, xlim = NULL, ylim = NULL, cex = 1, cex.legend = 1, 
                       main=NULL, grid=TRUE, mar=NULL){
  
  # example:
  # 
  # m <- matrix(c(3,4,5,1,5,4,2,6,2), nrow = 3, 
  #             dimnames = list(dose = c("A","B","C"), 
  #                             age = c("2000","2001","2002")))
  # PlotLinesA(m, col=rev(c(PalHelsana(), "grey")), main="Dosw ~ age", lwd=3, ylim=c(1,10))
  
  last <- rev(sort(tail(as.matrix(x), 1)))

  if(is.null(mar)) Mar(,,, 10)  # this would be nice, but there's no plot so far... max(strwidth(names(last))) * 1.2
  else do.call(Mar, as.list(mar))
  
  matplot(x, y, type="n", las=1, xlim=xlim, ylim=ylim, xaxt="n", main=main, ylab=ylab, cex = cex)
  axis(side = 1, at=c(1:nrow(x)), rownames(x), xlab=xlab)
  if(grid) grid()
  matplot(x, type="l", lty=lty, col=col, lwd=lwd, lend=lend, xaxt="n", add=TRUE)
  
  oldpar <- par(xpd=TRUE); on.exit(par(oldpar))
  mtext(text = names(last), side=4, line = 1.8, at = SpreadOut(last, mindist = 1.2 * strheight("M")), 
        las=1, cex = cex.legend)
  segments(x0 = par("usr")[2] + diff(par("usr")[1:2]) * 0.02, 
           x1 = par("usr")[2] + diff(par("usr")[1:2]) * 0.02 * 2, 
           y0 = SpreadOut(unlist(last), mindist = 1.2 * strheight("M")), 
           lwd=4, lend=1, 
           col=col[match(names(last), colnames(x))])

}
