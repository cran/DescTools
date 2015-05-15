PlotPar <-
function(){
  # plots the most used plot parameters

  usr <- par(no.readonly=TRUE);  on.exit(par(usr)) 
  
  if( !is.null(dev.list()) ){ 
    curwin <- dev.cur()
    on.exit({
      dev.set(curwin)
      par(usr)
      })
  } 
  
  # this does not work and CRAN does not allow windows()
  # dev.new(width=7.2, height=4)

  par( mar=c(0,0,0,0), mex=0.001, xaxt="n", yaxt="n", ann=F, xpd=TRUE)
  plot( x=1:25, y=rep(11,25), pch=1:25, cex=2, xlab="", ylab=""
      , frame.plot=FALSE, ylim=c(-1,15))
  points( x=1:25, y=rep(12.5,25), pch=1:35, cex=2, col="blue", bg="red")
  text( x=1:25, y=rep(9.5,25), labels=1:25, cex=0.8 )
  segments( x0=1, x1=4, y0=0:5, lty=6:1, lwd=3 )
  text( x=5, y=6:0, adj=c(0,0.5), labels=c("0 = blank", "1 = solid (default)", "2 = dashed", "3 = dotted", "4 = dotdash", "5 = longdash", "6 = twodash") )
  segments( x0=10, x1=12, y0=0:6, lty=1, lwd=7:1 )
  text( x=13, y=0:6, adj=c(0,0.5), labels=7:1 )
  points( x=rep(15,7), y=0:6, cex=rev(c(0.8,1,1.5,2,3,4,7)) )
  text( x=16, y=0:6, adj=c(0,0.5), labels=rev(c(0.8,1,1.5,2,3,4,7)) )
  text( x=c(1,1,10,15,18,18), y=c(14,7.5,7.5,7.5,7.5,2.5), labels=c("pch","lty","lwd","pt.cex","adj","col"), cex=1.3, col="grey40")
  adj <- expand.grid(c(0,0.5,1),c(0,0.5,1))
  for( i in 1:nrow(adj)  ){ 
    text( x=18+adj[i,1]*7, y=3.5+adj[i,2]*3, label=paste("text", paste(adj[i,], collapse=",") ), adj=unlist(adj[i,]), cex=0.8 )
  }
  points( x=18:25, y=rep(1,8), col=1:8, pch=15, cex=2 )
  text( x=18:25, y=0, adj=c(0.5,0.5), labels=1:8, cex=0.8 )  

}
