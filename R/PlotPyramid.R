PlotPyramid <-
function(lx, rx = NA, ylab = "",
            ylab.x = 0, col = c("red", "blue"), border = par("fg"),             
            main = "", lxlab = "", rxlab = "", xlim = NULL,
            gapwidth = NULL, xaxt = TRUE, 
            args.grid = NULL,
            cex.axis = par("cex.axis"), cex.lab = par("cex.axis"), cex.names = par("cex.axis"), adj = 0.5, ...) {

  if (missing(rx) && length(dim(lx)) > 0) {
    rx <- lx[, 2]
    lx <- lx[, 1]
  }
  
  b <- barplot(-lx, horiz=TRUE, plot=FALSE, ...)
  
  ylim <- c(0, max(b))
  if(is.null(xlim)) xlim <- c(-max(lx), max(rx))
  plot( 1, type="n", xlim=xlim, ylim=ylim, frame.plot=FALSE
        , xlab="", ylab="", axes=FALSE, main=main)
  if(is.null(gapwidth)) gapwidth <- max(strwidth(ylab, cex=cex.names)) + 3*strwidth("M", cex=cex.names)

  at.left <- axTicks(1)[axTicks(1)<=0] - gapwidth/2
  at.right <- axTicks(1)[axTicks(1)>=0] + gapwidth/2

  # grid: define default arguments 
  add.grid <- TRUE
  if(!is.null(args.grid)) if(all(is.na(args.grid))) {add.grid <- FALSE} 

  if(add.grid){
    args.grid1 <- list(col="grey", lty="dotted") 
    # override default arguments with user defined ones
    if (!is.null(args.grid)) { 
      args.grid1[names(args.grid)] <- args.grid 
    } 
    abline(v=c(at.left, at.right), col=args.grid1$col, lty=args.grid1$lty )
  }
  
  if(length(col) == 1) border <- rep(col, 2)
  lcol <- rep(col[seq_along(col) %% 2 == 1], times=length(lx))
  rcol <- rep(col[seq_along(col) %% 2 == 0], times=length(rx))

  if(length(border) == 1) border <- rep(border, 2)
  lborder <- rep(border[seq_along(border) %% 2 == 1], times=length(lx))
  rborder <- rep(border[seq_along(border) %% 2 == 0], times=length(rx))
  
  barplot(-lx, horiz=TRUE, col=lcol, add=T, axes=FALSE, names.arg="", 
          offset=-gapwidth/2, border=lborder, ...)
  barplot(rx, horiz=TRUE, col=rcol, add=T, axes=FALSE, names.arg="", 
          offset=gapwidth/2, border=rborder, ...)

  oldpar <- par(xpd=TRUE); on.exit(par(oldpar))
  
  ylab.x <- ylab.x + sign(ylab.x) * gapwidth/2
  text(ylab, x=ylab.x, y=b, cex=cex.names, adj = adj)

  if(!xaxt == "n"){
    axis(side=1, at=at.right, labels=axTicks(1)[axTicks(1)>=0], cex.axis=cex.axis)
    axis(side=1, at=at.left, labels=-axTicks(1)[axTicks(1)<=0], cex.axis=cex.axis)
  }
  
  mtext(text=rxlab, side=1, at=mean(at.right), padj=0.5, line=2.5, cex=cex.lab)
  mtext(text=lxlab, side=1, at=mean(at.left), padj=0.5, line=2.5, cex=cex.lab)
  
  return(b)   # return the same result as barplot
}
