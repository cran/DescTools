PlotFun <-
function(FUN, args=NULL, from=NULL, to=NULL, by=NULL, xlim=NULL, 
                    ylim = NULL, polar = FALSE, type="l",
                    col = par("col"), lwd= par("lwd"), lty=par("lty"), pch=NA,
                    add = FALSE, ...){
  
#   # all dot arguments
#   dot.args <- match.call(expand.dots=FALSE)$...
#   # the dot arguments which match PercTable.table
#   # pt.args <- dot.args[names(dot.args) %in% names(formals(PercTable.table))]
#   # the dot arguments which DO NOT match PercTable.table
#   par.args <- dot.args[names(dot.args) %nin% names(formals(PlotFun))]
  
  # see also Hmisc::minor.tick 
  
  
  vars <- all.vars(FUN)
  vars <- vars[vars %nin% names(args)]

  # this is not really smart ....
  if(is.null(from)) from <- -5
  if(is.null(to)) to <- 5
  if(is.null(by)) by <- (to - from) / 500
  
  
  # the independent variable
  assign(vars, seq(from = from, to = to, by=by))
  
  # define the parameters
  for(i in seq_along(args)) {
    assign(names(args)[i], unlist(args[i]))
    
    # this does not work:
    if(length(get(names(args)[i])) > 1) {
      assign(names(args)[i], get(names(args)[i])[1])
      warning(gettextf("first element used of '%s' argument", names(args)[i]))
    }
  }  
  
  # Inhibit model interpretation for function plot
  FUN[[2]] <-   as.formula("~" %c% gettextf("I(%s)", deparse(FUN[[2]])) )[[2]]
  FUN[[3]] <-   as.formula("~" %c% gettextf("I(%s)", deparse(FUN[[3]])) )[[2]]
  
  # this will evaluate in parent.frame(), so in function's env
  p <- ParseFormula(FUN)
  
  y <- p$lhs$mf.eval[,1]
  x <- p$rhs$mf.eval[,1]
  
  if(polar){
    cord <- PolToCart(r = y, theta = x)
    y <- cord$y
    x <- cord$x
  }

  if(is.null(xlim)){
    xlim <- range(pretty(range(x[is.finite(x)])))
  }
  if(is.null(ylim)){
    ylim <- range(pretty(range(y[is.finite(y)])))
  }
  
  # define plot parameters
  m <- match.call(expand.dots = FALSE) 
  m$...$frame.plot <- InDots(..., arg="frame.plot", default = FALSE)
  m$...$axes <- InDots(..., arg="axes", default = NULL)
  m$...$asp <- InDots(..., arg="asp", default = 1)
  m$...$xlab <- InDots(..., arg="xlab", default = "")
  m$...$ylab <- InDots(..., arg="ylab", default = "")
  if(is.null(m$...$axes)) {
    add.axes <- TRUE
    m$...$axes <- FALSE
  } else {
    add.axes <- FALSE
  }
  
  if(!add){
    do.call(plot, c(list(y=1, x=1, xlim=xlim, ylim=ylim, type="n"), m$...))
  }

  if(add.axes) {
    axis(1, pos = 0)
    # we set minor ticks for the axes, 4 ticks between 2 major ticks
    axp <- par("xaxp")
    axp[3] <- 5 * axp[3]
    axis(1, pos = 0, TRUE, at=axTicks(side=1, axp=axp), labels = NA, tck=-0.01)
    axis(2, pos = 0,las=1)
    axp <- par("yaxp")
    axp[3] <- 5 * axp[3]
    axis(2, pos = 0, TRUE, at=axTicks(side=1, axp=axp), labels = NA, tck=-0.01)
  }  
  
  lines(y=y, x=x, type=type, col=col, lty=lty, lwd=lwd, pch=pch)

}
