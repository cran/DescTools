PlotWeb <-
function(m, col=c("red","blue"), lty=par("lty"), args.legend=NULL, pch=21, pt.cex=2,
                    pt.col="black", pt.bg="darkgrey", ... ){

# following an idee from library(LIM)
# example(plotweb)

  oldpar <- par(c("lend","xpd"))
  on.exit(par(oldpar))
  
  w <- 4
  par("xpd"=TRUE, lend="butt")

  Canvas(w, ...)  
  angles <- seq(0, 2*pi, length=nrow(m)+1)[-1]
  xy <- PolToCart(r=3, theta=angles)
  cbind(1, (angles %[]% c(pi/2, 3*pi/2))*1)
  text(x=xy$x, y=round(xy$y,3), labels=colnames(m), pos=(!angles %[]% c(pi/2, 3*pi/2))*2 + 2, offset=1 )
  #text(x=xy$x, y=round(xy$y,3), labels="x", pos=(!angles %[]% c(pi/2, 3*pi/2))*2+2   )
  
  d.m <- data.frame( from=rep(colnames(m), nrow(m)), to=rep(colnames(m), each=nrow(m))
    , d=as.vector(m)
    , from.x=rep(xy$x, nrow(m)), from.y=rep(xy$y, nrow(m)), to.x=rep(xy$x, each=nrow(m)), to.y=rep(xy$y, each=nrow(m)) )
  # d.m <- d.m[d.m$d > 0,]
  # lineare transformation of linewidth
  a <- 1
  b <- 15
  d.m$d.sc <- (b-a) * (min(d.m$d)-a) + (b-a) /diff(range(d.m$d)) * d.m$d       
  
  d.m$d.sc <- LinScale(abs(d.m$d), newlow=0.5, newhigh=10 )
  col <- rep(col, length.out=2)
  segments( x0=d.m$from.x, y0=d.m$from.y, x1 = d.m$to.x, y1 = d.m$to.y,
         col = col[((sign(d.m$d)+1)/2)+1], lty = lty, lwd = d.m$d.sc, lend= 1)
  points( xy, cex=pt.cex, pch=pch, col=pt.col, bg=pt.bg )
  
  args.legend1 <- list( x="bottomright", inset=-0.05, legend=round(c(-min(abs(d.m$d)), max(abs(d.m$d))), 3)
                        , lwd = c(a,b), col=col, bg="white", cex=0.8)
  if ( !is.null(args.legend) ) { args.legend1[names(args.legend)] <- args.legend }
  add.legend <- TRUE
  if(!is.null(args.legend)) if(all(is.na(args.legend))) {add.legend <- FALSE} 
  
  if(add.legend) do.call("legend", args.legend1)
  
}
