PlotMonth <-
function(x, type = "l", labels, xlab = "", ylab = deparse(substitute(x)), ...)
#--
# Funktion fuer univariate Zeitreihen, zeichnet die Monats- oder Saisoneffekte
#
# von S+5 übernommen und an R angepasst
#
# x muss eine univariate Zeitreihe sein
#--

{
  if(length(dim(x)))
    stop("This implementation is only for univariate time series")
  old.opts <- options(warn = -1)
  
  on.exit(options(old.opts))
  
  if(!(type == "l" || type == "h"))
    stop(paste("type is \"", type, "\", it must be \"l\" or \"h\"",
               sep = ""))
  
  f <- frequency(x)
  cx <- cycle(x)
  m <- tapply(x, cx, mean)
  if(cx[1] != 1 || cx[length(x)] != f) {
    x <- ts(c(rep(NA, cx[1] - 1), x, rep(NA, f - cx[length(x)])),
            start = start(x, format = T)[1], end = c(end(x, format
                                                         = T)[1], f), frequency = f)
    cx <- cycle(x)
  }
  i <- order(cx)
  n <- length(x)
  if(missing(labels))
    labels <- if(f == 12) c("Jan", "Feb", "Mar", "Apr", "May",
                            "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    ) else if(f == 4)
      c("First", "Second", "Third", "Fourth")
  else 1:f
  if(length(labels) != f)
    stop(paste("There must be", f, "labels"))
  p <- n/f
  hx <- seq(1, n, by = p) + (0:(f - 1))
  hy <- rep(m, rep(2, length(m)))
  X <- as.vector(outer(0:(p - 1), hx, "+"))
  plot(c(1, n + f), range(x[!is.na(x)]), type = "n", axes = F, xlab = 
         xlab, ylab = ylab, ...)
  dotdot <- list(...)
  ddttl <- match(c("main", "sub", "axes", "ylim"), names(dotdot), nomatch
                 = 0)
  ddttl <- ddttl[ddttl != 0]
  add.axes <- T
  if(length(ddttl)) {
    if(any(names(dotdot) == "axes"))
      add.axes <- dotdot$axes
    dotdot <- dotdot[ - ddttl]
  }
  if(type == "l")
    for(j in 1:f)
      do.call("lines", c(list(hx[j]:(hx[j] + p - 1), x[i][
        ((j - 1) * p + 1):(j * p)]), dotdot))
  else if(type == "h")
    do.call("segments", c(list(X, x[i], X, m[cx][i]), dotdot))
  do.call("segments", c(list(hx, m, hx + p, m), dotdot)) 
  if(add.axes) {
    box()
    axis(2)
    axis(1, at = hx + p/2, labels = labels)
  }
  invisible()
}
