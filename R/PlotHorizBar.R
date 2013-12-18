PlotHorizBar <-
function (from, to, grp = 1, col = "lightgrey", border = "black", ...)  {

  if (is.null(dev.list()))  plot.new()

  grp <- factor(grp)
  plot(1, xlim = range(pretty((c(from, to)))), ylim = c(0, nlevels(grp) + 1), 
       type = "n", ylab = "", yaxt = "n", ...)
  xleft <- from
  xright <- to
  ytop <- as.numeric(grp) + 0.3
  ybottom <- as.numeric(grp) - 0.3
  rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, 
       col = col, border = border, lty = par("lty"), lwd = par("lwd"))
}
