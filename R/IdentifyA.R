IdentifyA <-
function(formula, data, subset, poly = FALSE){

  opt <- options(na.action=na.pass); on.exit(options(opt)) 
  
  # identifies points in a plot, lying in a rectangle, spanned by upleft, botright
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "na.action", "subset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  x <- mf[[-response]]
  y <- mf[[response]]

  vname <- attr(attr(attr(mf, "terms"), "dataClasses"), "names")
  
  if(poly){
    cat("Select polygon points and click on finish when done!\n")
    xy <- locator(type="n")
    polygon(xy, border="grey", lty="dotted")
    idx <- PtInPoly(data.frame(x, y), do.call("data.frame", xy))$pip == 1
    code <- paste("x %in% c(", paste(which(idx), collapse=","), ")", sep="")
  } else {
    cat("Select upper-left and bottom-right point!\n")
    xy <- locator(n=2, type="n")[1:2]
    rect(xy$x[1], xy$y[1], xy$x[2], xy$y[2], border="grey", lty="dotted")
    
    idx <- (x %[]% range(xy$x) & y %[]% range(xy$y))
    code <- paste(vname[2], " %[]% c(", xy$x[1], ", ", xy$x[2], ") & ", vname[1] ," %[]% c(",  xy$y[1], ", ", xy$y[2], "))", sep="")
  }
  
  res <- which(idx)
  xy <- lapply(lapply(xy, range), signif, digits=4)
  attr(x=res, which="cond") <- code
  
  return(res)
  
}
