PlotCandlestick <-
function(x, y, xlim = NULL, ylim = NULL, col = c("springgreen4","firebrick"), border=NA, args.grid = NULL, ...) {

  
  xlim <- if (is.null(xlim)) 
    range(x[is.finite(x)])
  else xlim
  ylim <- if (is.null(ylim)) 
    range(y[is.finite(y)])
  else ylim
  
  plot(x = 1, y = 1, xlim = xlim, 
    ylim = ylim, type = "n", xaxt = "n", xlab = "", ...)

  add.grid <- TRUE
  if(!is.null(args.grid)) if(all(is.na(args.grid))) {add.grid <- FALSE} 
  
  if (add.grid) {
    args.grid1 <- list(lty="solid", col="grey83")
    if (!is.null(args.grid)) {
      args.grid1[names(args.grid)] <- args.grid
    }
    do.call("grid", args.grid1)
  }
  
  # open low high close
  segments(x0 = x, y0 = y[,2], y1 = y[,3], col = col[(y[,1] > y[,4]) * 1 + 1])
  rect(xleft = x - 0.3, ybottom = y[,1], xright = x + 0.3, ytop = y[, 4], 
    col = col[(y[,1] > y[,4]) * 1 + 1], border = border)
    
  axis(side = 1, at = x, labels = x)
  
}
