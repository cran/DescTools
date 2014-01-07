PlotArea.default <-
function(x, y=NULL, prop=FALSE, add=FALSE, xlab=NULL, ylab=NULL, 
                             col=NULL, frame.plot=FALSE, ...) {

  if(is.ts(x)) {  # ts/mts 
    if(is.null(ylab)) ylab <- deparse(substitute(x))
    x <- data.frame(Time=time(x), x)
  }

  if(is.table(x)) { # table 
    if(is.null(ylab)) ylab <- deparse(substitute(x))
    if(length(dim(x)) == 1)
      x <- t(t(unclass(x)))
    else
      x <- unclass(x)
  }

  if(is.matrix(x)) { # matrix 
    if(!is.null(rownames(x)) && !any(is.na(suppressWarnings(as.numeric(rownames(x)))))) {
      x <- data.frame(as.numeric(rownames(x)), x)
      names(x)[1] <- ""
    } else {
      x <- data.frame(Index=seq_len(nrow(x)), x)
    }
  }

  if(is.list(x)) { # data.frame or list 
    if(is.null(xlab))  xlab <- names(x)[1]
    if(is.null(ylab)) {
      if(length(x) == 2)
        ylab <- names(x)[2]
      else
        ylab <- ""
    }

    y <- x[-1]
    x <- x[[1]]
  }

  if(is.null(y)) { # one numeric vector passed, plot it on 1:n 
    if(is.null(xlab))  xlab <- "Index"
    if(is.null(ylab))  ylab <- deparse(substitute(x))

    y <- x
    x <- seq_along(x)
  }

  if(is.null(xlab))  xlab <- deparse(substitute(x))
  if(is.null(ylab))  ylab <- deparse(substitute(y))
  
  y <- as.matrix(y)

  if(is.null(col))  col <- gray.colors(ncol(y))
  col <- rep(col, length.out=ncol(y))

  if(prop)  y <- prop.table(y, 1)

  y <- t(rbind(0, apply(y, 1, cumsum)))
  na <- is.na(x) | apply(is.na(y),1,any)
  x <- x[!na][order(x[!na])]
  y <- y[!na,][order(x[!na]),]

  if(!add)  suppressWarnings(matplot(x, y, type="n", xlab=xlab, ylab=ylab, frame.plot=frame.plot, ...))
  xx <- c(x, rev(x))

  for(i in 1:(ncol(y)-1)) {
    yy <- c(y[,i+1], rev(y[,i]))
    suppressWarnings(polygon(xx, yy, col=col[i], ...))
  }

  invisible(y[,-1])
}
