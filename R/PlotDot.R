PlotDot <-
function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
                     pch = 21, gpch = 21, bg = par("bg"), color = par("fg"), gcolor = par("fg"), 
                     lcolor = "gray", xlim = NULL, main = NULL, 
                     xlab = NULL, ylab = NULL, add = FALSE, 
                     args.errbars = NULL, ...) {
  
  # this is mainly R-Core code from dotchart
  # extended by argument add and returning y-coordinates
  
  x <- Rev(x, margin=1)
  
  if(is.null(xlim)) {
    if(is.null(args.errbars)){
      xlim <- range(x[is.finite(x)])
    } else {
      rng <- c(args.errbars[["from"]], args.errbars[["to"]])
      xlim <- range(rng[is.finite(rng)])
    }  
  }
  
  opar <- par("mai", "mar", "cex", "yaxs")
  on.exit(par(opar))
  par(cex = cex, yaxs = "i")
  if (!is.numeric(x)) 
    stop("'x' must be a numeric vector or matrix")
  n <- length(x)
  if (is.matrix(x)) {
    if (is.null(labels)) 
      labels <- rownames(x)
    if (is.null(labels)) 
      labels <- as.character(1L:nrow(x))
    labels <- rep_len(labels, n)
    if (is.null(groups)) 
      groups <- col(x, as.factor = TRUE)
    glabels <- levels(groups)
  }
  else {
    if (is.null(labels)) 
      labels <- names(x)
    glabels <- if (!is.null(groups)) 
      levels(groups)
    if (!is.vector(x)) {
      warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
      x <- as.numeric(x)
    }
  }
  if(!add) plot.new()
  linch <- if (!is.null(labels)) 
    max(strwidth(labels, "inch"), na.rm = TRUE)
  else 0
  if (is.null(glabels)) {
    ginch <- 0
    goffset <- 0
  }
  else {
    ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
    goffset <- 0.4
  }
  if (!(is.null(labels) && is.null(glabels))) {
    nmai <- par("mai")
    nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
      0.1
    par(mai = nmai)
  }
  if (is.null(groups)) {
    o <- 1L:n
    y <- o
    ylim <- c(0, n + 1)
  }
  else {
    o <- sort.list(as.numeric(groups), decreasing = TRUE)
    x <- x[o]
    groups <- groups[o]
    color <- rep_len(color, length(groups))[o]
    lcolor <- rep_len(lcolor, length(groups))[o]
    offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
    y <- 1L:n + 2 * offset
    ylim <- range(0, y + 2)
  }
  if(!add) plot.window(xlim = xlim, ylim = ylim, log = "")
  lheight <- par("csi")
  if (!is.null(labels)) {
    linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
    loffset <- (linch + 0.1)/lheight
    labs <- labels[o]
    mtext(labs, side = 2, line = loffset, at = y, adj = 0, 
          col = color, las = 2, cex = cex, ...)
  }
  if(!add) abline(h = y, lty = "dotted", col = lcolor)
  points(x, y, pch = pch, col = color, bg = bg)
  if (!is.null(groups) & (!add)) {
    gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
                         2) - 1)
    ginch <- max(strwidth(glabels, "inch", font=2), na.rm = TRUE)
    goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
    mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, font=2,
          col = gcolor, las = 2, cex = cex, ...)
    if (!is.null(gdata)) {
      abline(h = gpos, lty = "dotted")
      points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
             ...)
    }
  }
  if(!add) axis(1)
  if(!add) box()
  if(!add) title(main = main, xlab = xlab, ylab = ylab, ...)
  
  # add error bars if requested
  args.errbars1 <- list(horiz=TRUE, pos=y)
  if(!is.null(args.errbars)) {
    args.errbars1[names(args.errbars)] <- args.errbars
    args.errbars1[["from"]] <- Rev(args.errbars1[["from"]], c(1,2))
    args.errbars1[["to"]] <- Rev(args.errbars1[["to"]], c(1,2))
    args.errbars1[["mid"]] <- Rev(args.errbars1[["mid"]], c(1,2))
    
    do.call("ErrBars", args.errbars1)
  }
  
  
  # return y-values
  invisible(y)
  
}
