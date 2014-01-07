PlotViolin.default <-
function (x, ..., horizontal = FALSE, bw = "SJ", na.rm = FALSE
                                , names = NULL, args.boxplot = NULL)  {

  # Make a simple violin plot call from violinplot. values are x,y to plot 
  vlnplt <-  function(x, y, center, horizontal = FALSE, 
                      col = NA , border = par("fg"), lty = 1, lwd = 1, 
                      density = NULL, angle = 45, fillOddEven = FALSE, ...) {
      # double up first
      x <- c(x, rev(x))
      y <- c(y, -rev(y))
      y <- y + center
      
      # swap x and y if horizontal
      if (horizontal == FALSE) { tmp=x; x=y; y=tmp }

      polygon(x=x, y=y, border=border, col=col, lty=lty, lwd=lwd, 
              density=density, angle=angle, fillOddEven=fillOddEven, ...) 
    }

  
  # main *****************  
  
  m <- match.call(expand.dots = FALSE) 
  pars <- m$...[ names(m$...)[!is.na(match(names(m$...), c(
    "cex","cex.axis","cex.lab","cex.main","cex.sub","col.axis","col.lab","col.main","col.sub","family",
    "font","font.axis","font.lab","font.main","font.sub","las","tck","tcl","xaxt","xpd","yaxt"
  )))]]
  oldpar <- par(pars); on.exit(par(oldpar))

  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names)) 
                 attributes(args)$names != ""
               else 
                 rep(FALSE, length = length(args))

  groups <- if(is.list(x)) x else args[!namedargs]

  if (0 == (n <- length(groups))) 
      stop("invalid first argument")
  if (length(class(groups))) 
      groups <- unclass(groups)
  if (!missing(names)) 
      attr(groups, "names") <- names
  else {
      if (is.null(attr(groups, "names"))) 
          attr(groups, "names") <- 1:n
      names <- attr(groups, "names")
  }

  xvals <- matrix(0, nrow = 512, ncol = n)
  yvals <- matrix(0, nrow = 512, ncol = n)
  center <- 1:n
  for (i in 1:n) {
      if(na.rm) xi <- na.omit(groups[[i]]) 
        else xi <- groups[[i]]
      tmp.dens <- density(xi, bw = bw)
      xvals[, i] <- tmp.dens$x
      yvals.needtoscale <- tmp.dens$y
      yvals.scaled <- 7/16 * yvals.needtoscale / max(yvals.needtoscale)
      yvals[, i] <- yvals.scaled
  }
  if (horizontal == FALSE) {
      xrange <- c(1/2, n + 1/2)
      yrange <- range(xvals)
  }
  else {
      xrange <- range(xvals)
#      yrange <- c(min(yvals), max(yvals))
      yrange <- c(1/2, n + 1/2)
  }


  plot.args <- m$...[names(m$...)[!is.na(match(names(m$...), 
     c("xlim","ylim","main","xlab","ylab","panel.first","panel.last","frame.plot","add")))]]
  if(! "xlim" %in% names(plot.args)) plot.args <- c(plot.args, list(xlim=xrange))
  if(! "ylim" %in% names(plot.args)) plot.args <- c(plot.args, list(ylim=yrange))
  if(! "xlab" %in% names(plot.args)) plot.args <- c(plot.args, list(xlab=""))
  if(! "ylab" %in% names(plot.args)) plot.args <- c(plot.args, list(ylab=""))
  if(! "frame.plot" %in% names(plot.args)) plot.args <- c(plot.args, list(frame.plot=TRUE))
  
  # plot only if add is not TRUE
  if(! "add" %in% names(plot.args)) add <- FALSE else add <- plot.args$add
  if(!add) do.call(plot, c(plot.args, list(x=0, y=0, type="n", axes=FALSE)))
  
  # poly.args <- m$...[names(m$...)[!is.na(match(names(m$...), c("border","col","lty","density","angle","fillOddEven")))]]
  # neu:
  poly.args <- args[names(args)[!is.na(match(names(args), c("border","col","lty","lwd","density","angle","fillOddEven")))]]
  poly.args <- lapply( poly.args, rep, length.out=n )
  
  for (i in 1:n) 
#      do.call(vlnplt, c(poly.args[i], list(x=xvals[, i]), list(y=yvals[, i]), 
#                        list(center=center[i]), list(horizontal = horizontal)))
      do.call(vlnplt, c(lapply(poly.args, "[", i), list(x=xvals[, i]), list(y=yvals[, i]), 
                        list(center=center[i]), list(horizontal = horizontal)))

  axes <- Coalesce(unlist(m$...[names(m$...)[!is.na(match(names(m$...), c("axes")))]]), TRUE)
  if(axes){
    xaxt <- Coalesce(unlist(m$...[names(m$...)[!is.na(match(names(m$...), c("xaxt")))]]), TRUE)
    if(xaxt!="n") if(horizontal == TRUE) axis(1) else axis(1, at = 1:n, labels = names)

    yaxt <- Coalesce(unlist(m$...[names(m$...)[!is.na(match(names(m$...), c("yaxt")))]]), TRUE)
    if(yaxt!="n") if(horizontal == TRUE)  axis(2, at = 1:n, labels = names) else axis(2)  
  }
  
  if(!identical(args.boxplot, NA)){
    
    args1.boxplot <- list(col="black", add=TRUE, boxwex=0.05, axes=FALSE,
       outline=FALSE, whisklty=1, staplelty=0, medcol="white")
    args1.boxplot[names(args.boxplot)] <- args.boxplot
    
    do.call(boxplot, c(list(x, horizontal = horizontal), args1.boxplot))
    
  }
  
}
