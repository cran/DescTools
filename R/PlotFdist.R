PlotFdist <-
function (x, main = deparse(substitute(x)), xlab = ""
                        , xlim = NULL
                        , do.hist = !(all(IsWhole(x,na.rm=TRUE)) & length(unique(na.omit(x))) < 13)
                        # do.hist overrides args.hist, add.dens and rug
                        , args.hist = NULL          # list( breaks = "Sturges", ...)
                        , args.rug = NA             # list( ticksize = 0.03, side = 1, ...), pass NA if no rug
                        , args.dens = NULL          # list( bw = "nrd0", col="#9A0941FF", lwd=2, ...), NA for no dens  
                        , args.curve = NA          # list( ...), NA for no dcurve  
                        , args.boxplot = NULL       # list( pars=list(boxwex=0.5), ...), NA for no boxplot
                        , args.ecdf = NULL          # list( col="#8296C4FF", ...), NA for no ecdf
                        , heights = NULL            # heights (hist, boxplot, ecdf) used by layout
                        , pdist = NULL              # distances of the plots, default = 0                        
                        , na.rm = FALSE, cex.axis = NULL, cex.main = NULL ) {
  
  # Plot function to display the distribution of a cardinal variable
  # combines a histogram with a density curve, a boxplot and an ecdf 
  # rug can be added by using add.rug = TRUE
  
  # default colors are Helsana CI-colors
  
  # dev question: should dots be passed somewhere??
  
  usr <- par(no.readonly=TRUE);  on.exit(par(usr)) 
  
  add.boxplot <- !identical(args.boxplot, NA)
  add.rug <- !identical(args.rug, NA)
  add.dens <- !identical(args.dens, NA)
  add.ecdf <- !identical(args.ecdf, NA)
  add.dcurve <- !identical(args.curve, NA)
  
  # preset heights
  if(is.null(heights)){
    if(add.boxplot) { 
      if(add.ecdf) heights <- c(2, 0.5, 1.4)
      else heights <- c(2, 1.4)
    } else { 
      if(add.ecdf) heights <- c(2, 1.4)
    }
  }
  
  if(is.null(pdist)) {
    if(add.boxplot) pdist <- c(0, 0)
    else pdist <- c(0, 1)
  }  
  
  if (add.ecdf && add.boxplot) {
    layout(matrix(c(1, 2, 3), nrow = 3, byrow = TRUE), heights = heights, TRUE)
    if(is.null(cex.axis)) cex.axis <- 1.3
    if(is.null(cex.main)) cex.main <- 1.7
  } else {
    if((add.ecdf || add.boxplot)) {
      layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), heights = heights[1:2], TRUE)
      if(is.null(cex.axis)) cex.axis <- 0.9
    } else {
      if(is.null(cex.axis)) cex.axis <- 0.95
    }
  }
  
  # plot histogram, change margin if no main title
  par(mar = c(ifelse(add.boxplot || add.ecdf, 0, 5.1), 6.1, 1, 2.1))
  if(!is.na(main)) { par(oma=c(0,0,3,0)) }  
  
  # wait for omitting NAs until all arguments are evaluated, e.g. main...
  if(na.rm) x <- na.omit(x) 
  
  # handle open list of arguments: args.legend in barplot is implemented this way...
  # we need histogram anyway to define xlim
  args.hist1 <- list(x = x, xlab = "", ylab = "", freq = FALSE, 
                     xaxt = ifelse(add.boxplot || add.ecdf, "n", "s"), xlim = xlim, ylim = NULL, main = NA, las = 1, 
                     col = "white", border = "grey70", cex.axis = cex.axis)
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
                         col = getOption("col1", hred), lwd = 2, lty = "solid")
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
    
    
    # plot special distribution curve 
    if (add.dcurve) {
      # preset default values
      args.curve1 <- list(expr = parse(text = gettextf("dnorm(x, %s, %s)", mean(x), sd(x))), 
                           add = TRUE,
                           n = 500, col = getOption("col3", hgreen), lwd = 2, lty = "solid")
      if (!is.null(args.curve)) {
        args.curve1[names(args.curve)] <- args.curve
      }
      
      if (is.character(args.curve1$expr)) args.curve1$expr <- parse(text=args.curve1$expr)
      
      do.call("curve", args.curve1)
      
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
         xaxt = "n", xlim = args.hist1$xlim, main = NA, 
         frame.plot = FALSE, las = 1, cex.axis = cex.axis, panel.first = {
           abline(h = axTicks(2), col = "grey", lty = "dotted")
           abline(h = 0, col = "black")
         })
  }
  
  # boxplot
  if(add.boxplot){
    par(mar = c(ifelse(add.ecdf, 0, 5.1), 6.1, pdist[1], 2.1))
    args.boxplot1 <- list(x = x, frame.plot = FALSE, main = NA, boxwex = 1, 
                          horizontal = TRUE, ylim = args.hist1$xlim,
                          at = 1, xaxt = ifelse(add.ecdf, "n", "s"), 
                          outcex = 1.3, outcol = rgb(0,0,0,0.5), cex.axis=cex.axis)
    if (!is.null(args.boxplot)) {
      args.boxplot1[names(args.boxplot)] <- args.boxplot
    }
    do.call("boxplot", args.boxplot1)
  }
  
  # plot ecdf
  if (add.ecdf) {
    par(mar = c(5.1, 6.1, pdist[2], 2.1))
    args.ecdf1 <- list(x = x, frame.plot = FALSE, main = NA, 
                       xlim = args.hist1$xlim, col = getOption("col1", hblue), lwd = 2, 
                       xlab = xlab, yaxt = "n", ylab = "", verticals = TRUE, 
                       do.points = FALSE, cex.axis = cex.axis)
    if (!is.null(args.ecdf)) {
      args.ecdf1[names(args.ecdf)] <- args.ecdf
    }
    do.call("plot.ecdf", args.ecdf1)
    axis(side = 2, at = seq(0, 1, 0.25), labels = gsub(pattern = "0\\.", 
                                                       replacement = " \\.", format(seq(0, 1, 0.25), 2)), 
         las = 1, xaxs = "e", cex.axis = cex.axis)
    abline(h = c(0.25, 0.5, 0.75), col = "grey", lty = "dotted")
    grid(ny = NA)
    points(x=range(x), y=c(0,1), col=args.ecdf1$col, pch=3, cex=2)
    
  }
  
  if(!is.na(main)) { 
    if(!is.null(cex.main)) par(cex.main=cex.main)
    title(main=main, outer = TRUE) 
  }  
  
  layout(matrix(1))           # reset layout on exit
}
