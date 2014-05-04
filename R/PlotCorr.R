PlotCorr <-
function(x, cols = colorRampPalette(c("red", "white", "blue"), space = "rgb")(20)
  , breaks = seq(-1, 1, length = length(cols)+1), border=NA, lwd=1
  , args.colorlegend = NULL, xaxt = par("xaxt"), yaxt = par("yaxt"), cex.axis = 0.8, las = 2        
  , mar = c(3,8,8,8), ...){
  
  # example:
  # m <- cor(d.pizza[,WhichNumerics(d.pizza)][,1:5], use="pairwise.complete.obs")
  # PlotCorr(m)
  # PlotCorr(m, args.colorlegend="n", las=1)
  # PlotCorr(m, cols=colorRampPalette(c("red", "white", "blue"), space = "rgb")(4), args.colorlegend=list(xlab=sprintf("%.1f", seq(1,-1, length=5))) )
  # PlotCorr(m, cols=colorRampPalette(c("red", "black", "green"), space = "rgb")(10))

  # PlotCorr(round(CramerV(d.pizza[,c("driver","operator","city", "quality")]),3))

  pars <- par(mar=mar); on.exit(par(pars))

  x <- x[,ncol(x):1] 
  image(x=1:nrow(x), y=1:ncol(x), xaxt="n", yaxt="n", z=x, frame.plot=FALSE, xlab="", ylab=""
    , col=cols, breaks=breaks, ... )
  if(xaxt!="n") axis(side=3, at=1:nrow(x), labels=rownames(x), cex.axis=cex.axis, las=las, lwd=-1) 
  if(yaxt!="n") axis(side=2, at=1:ncol(x), labels=colnames(x), cex.axis=cex.axis, las=las, lwd=-1) 

  if((is.list(args.colorlegend) || is.null(args.colorlegend))){
    args.colorlegend1 <- list( labels=sprintf("%.1f", seq(1,-1, length=length(cols)+1))
      , x=nrow(x)+0.5 + nrow(x)/20, y=ncol(x)+0.5
      , width=nrow(x)/20, height=ncol(x), cols=cols, cex=0.8 )
    if ( !is.null(args.colorlegend) ) { args.colorlegend1[names(args.colorlegend)] <- args.colorlegend }

    do.call("ColorLegend", args.colorlegend1)
  }
  
  if(!is.na(border)) {
    usr <- par("usr")
    rect(xleft=0.5, xright=nrow(x)+0.5, ybottom=0.5, ytop=nrow(x)+0.5, 
         lwd=lwd, border=border)
    usr <- par("usr")
    clip(0.5, nrow(x)+0.5, 0.5, nrow(x)+0.5)
    abline(h=seq(-2, nrow(x)+1,1)-0.5, v=seq(1,nrow(x)+1,1)-0.5, col=border,lwd=lwd)
    do.call("clip", as.list(usr))
  }
    
}
