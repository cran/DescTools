PlotMarDens <-
function( x, y, grp=1, xlim = NULL, ylim = NULL
  , col = rainbow(nlevels(factor(grp))) 
  , mardens = c("all","x","y"), pch=1, pch.cex=1.0, main=""
  , na.rm = FALSE, args.legend = NULL 
  , args.dens = NULL, ...){
  
  usr <- par("usr");  on.exit( par(usr) ) 

  mardens <- match.arg(arg = mardens, choices = c("all", "x", "y"))
  
  par(oma=c(0,0,3,0))

  d.frm <- data.frame(x=x, y=y, grp=grp)
  pch=rep(pch, length.out=nlevels(factor(grp)))    # recycle pch

  # this is plot.default defaults
  xlim <- if (is.null(xlim)) range(x[is.finite(x)]) else xlim
  ylim <- if (is.null(ylim)) range(y[is.finite(y)]) else ylim

  switch( mardens
    , "all" = { nf <- layout(matrix(c(2,0,1,3),2,2, byrow=TRUE), widths=c(9,1.5), heights=c(0.8,4), TRUE) }
    , "x" = { nf <- layout(matrix(c(2,1), 2,1, byrow=TRUE), c(9), c(0.8,4), TRUE) }
    , "y" =  { nf <- layout(matrix(c(1,2),1,2, byrow=TRUE), c(9,1.5), c(4), TRUE) }
  )

  par(mar=c(5,5,1,1))
  plot(x=d.frm$x, y=d.frm$y, xlim=xlim, ylim=ylim, type="n", ... )

  s <- split(d.frm[,1:2], d.frm$grp)
  for( i in seq_along(s)  ){
    points( x=s[[i]]$x, y=s[[i]]$y, col=col[i], pch=pch[i], cex=pch.cex)
  }


  args.legend1 <- list( x = "topright", inset = 0.02, legend = levels(factor(grp))
    , col = col, pch = pch, bg = "white", cex = 0.8 )
  if ( !is.null(args.legend) ) {
    if(!all(is.na(args.legend))){
      args.legend1[names(args.legend)] <- args.legend 
    } else { 
      args.legend1 <- NA 
    }  
  }

  if(!all(is.na(args.legend1))) do.call("legend", args.legend1)
  
  if(mardens %in% c("all","x")){
    par(mar=c(0,5,0,1))
    
    args.plotdens1 <- list(x = split(d.frm$x, d.frm$grp), na.rm = TRUE, 
                       col = col, xlim = xlim, axes=FALSE, 
                       args.legend = NA, xlab="", ylab="")
    if (!is.null(args.dens)) {
      args.plotdens1[names(args.dens)] <- args.dens
    }
    args.dens1 <- list(n = 4096, bw = "nrd0", kernel = "epanechnikov")
    if (!is.null(args.dens)) {
      ovr <- names(args.dens)[names(args.dens) %in% names(args.dens1)]
      args.dens1[ovr] <- args.dens[ovr]
    }
    args.plotdens1$args.dens <- args.dens1
    args.plotdens1 <- args.plotdens1[names(args.plotdens1) %nin% names(args.dens1)]
    
    do.call("PlotMultiDens", args.plotdens1)
    
#    PlotMultiDens( split(d.frm$x, d.frm$grp), col=col, na.rm=TRUE, xlim=xlim
#      , axes=FALSE, args.legend = NA, xlab="", ylab="" )
  }
  
  if(mardens %in% c("all","y")){
    par(mar=c(5,0,1,1))
    args.plotdens1 <- list(x = split(d.frm$y, d.frm$grp), na.rm = TRUE, 
                           col = col, ylim = ylim, axes=FALSE, flipxy=TRUE, 
                           args.legend = NA, xlab="", ylab="")
    if (!is.null(args.dens)) {
      args.plotdens1[names(args.dens)] <- args.dens
    }
    args.dens1 <- list(n = 4096, bw = "nrd0", kernel = "epanechnikov")
    if (!is.null(args.dens)) {
      ovr <- names(args.dens)[names(args.dens) %in% names(args.dens1)]
      args.dens1[ovr] <- args.dens[ovr]
    }
    args.plotdens1$args.dens <- args.dens1
    args.plotdens1 <- args.plotdens1[names(args.plotdens1) %nin% names(args.dens1)]

    do.call("PlotMultiDens", args.plotdens1)
#     PlotMultiDens( split(d.frm$y, d.frm$grp), col=col, na.rm=TRUE, ylim=ylim
#       , axes = FALSE, args.legend = NA, flipxy=TRUE, xlab="", ylab="" )

  }
  title(main=main, outer=TRUE)  

}
