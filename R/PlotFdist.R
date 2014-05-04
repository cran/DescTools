PlotFdist <-
function (x, main = deparse(substitute(x)), xlab = ""
  , xlim = NULL
  , do.hist = !(all(IsWhole(x,na.rm=TRUE)) & length(unique(na.omit(x))) < 13)
                              # do.hist overrides args.hist, add.dens and rug
  , args.hist = NULL          # list( breaks = "Sturges", ...)
  , args.rug = NA             # list( ticksize = 0.03, side = 1, ...), pass NA if no rug
  , args.dens = NULL          # list( bw = "nrd0", col="#9A0941FF", lwd=2, ...), NA for no dens  
  , args.boxplot = NULL       # list( pars=list(boxwex=0.5), ...), NA for no boxplot
  , args.ecdf = NULL          # list( col="#8296C4FF", ...), NA for no ecdf
  , heights = NULL  # heights (hist, boxplot, ecdf) used by layout
  , pdist = c(0, 0)           # distances of the plots, default = 0                        
  , na.rm = FALSE ) {
 
  # Plot function to display the distribution of a cardinal variable
  # combines a histogram with a density curve, a boxplot and an ecdf 
  # rug can be added by using add.rug = TRUE

  # default colors are Helsana CI-colors
  
  # dev question: should dots be passed somewhere??
  
  usr <- par("usr");  on.exit( par(usr) ) 
  
  add.rug <- TRUE
  if(!is.null(args.rug)) if(all(is.na(args.rug))) {add.rug <- FALSE} 
  add.dens <- TRUE
  if(!is.null(args.dens)) if(all(is.na(args.dens))) {add.dens <- FALSE} 
  add.ecdf <- TRUE
  if(!is.null(args.ecdf)) if(all(is.na(args.ecdf))) {add.ecdf <- FALSE} 
  
  if(is.null(heights)){
    if(add.ecdf) { heights <- c(2, 0.5, 1.4)}
    else         { heights <- c(2, 1.5)     }
  }
  
  if (add.ecdf == TRUE) {
    layout(matrix(c(1, 2, 3), nrow = 3, byrow = TRUE), heights = heights, TRUE)
  } else {
    layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), heights = heights[1:2], TRUE)
  }

  
  # plot histogram, change margin if no main title
  par(mar = c(0, 5.1, ifelse(main == "", 1.1, 4.1), 2.1))

  # wait for omitting NAs until all arguments are evaluated, e.g. main...
  if(na.rm) x <- na.omit(x) 
  
  # handle open list of arguments: args.legend in barplot is implemented this way...
  # we need histogram anyway to define xlim
  args.hist1 <- list(x = x, xlab = "", ylab = "", freq = FALSE, 
      xaxt = "n", xlim = xlim, ylim = NULL, main = main, las = 1, 
      col = "white", border = "grey70", cex.axis = 1.2)
  if (!is.null(args.hist)) {
      args.hist1[names(args.hist)] <- args.hist
  }
  x.hist <- do.call("hist", c(args.hist1[names(args.hist1) %in% 
      c("x", "breaks", "include.lowest", "right", "nclass")], plot = FALSE))
  x.hist$xname <- deparse(substitute(x))
  if (is.null(xlim))  args.hist1$xlim <- range(pretty(x.hist$breaks))
  args.histplot <- args.hist1[!names(args.hist1) %in% c("x", "breaks", "include.lowest", "right", "nclass")]

  if (do.hist) {
    # calculate max ylim for density curve, provided there should be one...
    # what's the maximal value in density or in histogramm$densities?

    # plot density
    if (add.dens) {
      # preset default values

      args.dens1 <- list(x = x, 
          col = "#9A0941FF", lwd = 2, lty = "solid")
      if (!is.null(args.dens)) {
          args.dens1[names(args.dens)] <- args.dens
      }
      x.dens <- do.call("density", args.dens1[-match(c("col", 
          "lwd", "lty"), names(args.dens1))])

      # overwrite the ylim if there's a larger density-curve    
      args.histplot[["ylim"]] <- range(pretty(c(0, max(c(x.dens$y, x.hist$density)))))
    }
    do.call("plot", append(list(x.hist), args.histplot))
      
    if (add.dens) {
        lines(x.dens, col = args.dens1$col, lwd = args.dens1$lwd, lty = args.dens1$lty)
    }
      
    if (add.rug) {
        args.rug1 <- list(x = x, col = "grey")
        if (!is.null(args.rug)) {
            args.rug1[names(args.rug)] <- args.rug
        }
        do.call("rug", args.rug1)
    }
  }
  else {
      plot(prop.table(table(x)), type = "h", xlab = "", ylab = "", 
          xaxt = "n", xlim = args.hist1$xlim, main = main, 
          frame.plot = FALSE, las = 1, cex.axis = 1.2, panel.first = {
              abline(h = axTicks(2), col = "grey", lty = "dotted")
              abline(h = 0, col = "black")
          })
  }

  # boxplot
  par(mar = c(ifelse(add.ecdf, 0, 5.1), 5.1, pdist[1], 2.1))
  args.boxplot1 <- list(x = x, frame.plot = FALSE, main = "", boxwex = 1, 
                        horizontal = TRUE, ylim = args.hist1$xlim,
                        at = 1, xaxt = "n", outcex = 1.3, outcol = rgb(0,0,0,0.5))
  if (!is.null(args.boxplot)) {
    args.boxplot1[names(args.boxplot)] <- args.boxplot
  }
  do.call("boxplot", args.boxplot1)
  
  # plot ecdf
  if (add.ecdf == TRUE) {
      par(mar = c(5.1, 5.1, pdist[2], 2.1))
      args.ecdf1 <- list(x = x, frame.plot = FALSE, main = "", 
          xlim = args.hist1$xlim, col = "#8296C4FF", lwd = 2, 
          xlab = xlab, yaxt = "n", ylab = "", verticals = TRUE, 
          do.points = FALSE, cex.axis = 1.2)
      if (!is.null(args.ecdf)) {
          args.ecdf1[names(args.ecdf)] <- args.ecdf
      }
      do.call("plot.ecdf", args.ecdf1)
      axis(side = 2, at = seq(0, 1, 0.25), labels = gsub(pattern = "0\\.", 
          replacement = " \\.", format(seq(0, 1, 0.25), 2)), 
          las = 1, xaxs = "e", cex.axis = 1.2)
      abline(h = c(0.25, 0.5, 0.75), col = "grey", lty = "dotted")
      grid(ny = NA)
      points(x=range(x), y=c(0,1), col=args.ecdf1$col, pch=3, cex=2)
      
  }
  layout(matrix(1))           # reset layout on exit
}
