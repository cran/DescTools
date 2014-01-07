PlotDotCI <-
function( x, xlim = NULL, pch = 21, pch.cex = 1.0, pch.col = "black", pch.bg = "grey50"
  , lcol = "grey40", lwd = 2, args.legend = NULL , code = 3
  , mar = c(7.1,4.1,4.1,2.1), ... ) {

  # example:
  # xx <- do.call("rbind",tapply( d.pizza$temperature, d.pizza$driver, MeanCI, na.rm=TRUE ) )
  # rownames(xx) <- levels(d.pizza$driver)
  # PlotDotCI( xx, pch.cex=1.2 )
  
  usr <- par("usr");  on.exit( par(usr) ) 
  par(mar=mar)
  
  # we need xlim here, as all segments should be completely displayed 
  if( missing("xlim") ) xlim <- range(pretty(x))
  
  x <- x[rev(1:nrow(x)),]    # reverse order by default
  
  dotchart( x[,1], xaxt="n", xlim=xlim, color="black", ...)
  abline(v=axTicks(1), col="grey", lty="dotted")
  arrows( x0=x[,2], x1=x[,3], y0=1:nrow(x), col=lcol, lwd=lwd, angle=90, code=code, length=0.05 )
  points( x=x[,1], y=1:nrow(x), bg=pch.bg, pch=pch, cex=pch.cex, col=pch.col )
  
  # the default values for the legend
  args.legend1 <- list( x = mean(par()$usr[1:2]), y = -1.3
        , legend = c("estimate", "95%-CI"), xpd = TRUE
        , ncol = 2, seg.len = 2, xjust = 0.5
        , adj = c(0, 0.5)
        , pt.bg = c(pch.bg, NA), lwd = lwd
        , pch = c(pch, NA), col = c(pch.bg, lcol)
        , bg = "white", cex = 0.8
      )
  if( length(unique(lwd))>1 ) {
    args.legend1[["fill"]] <-  NULL
    args.legend1[["col"]] <- col
    args.legend1[["lwd"]] <- lwd
  }  
  if ( !is.null(args.legend) ) { args.legend1[names(args.legend)] <- args.legend }
  add.legend <- TRUE
  if(!is.null(args.legend)) if(all(is.na(args.legend))) {add.legend <- FALSE} 
  
  if(add.legend) do.call("legend", args.legend1)

}
