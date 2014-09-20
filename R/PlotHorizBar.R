PlotHorizBar <-
function (from, to, grp = 1, col = "lightgrey", border = "black", 
                          height = 0.6, add = FALSE, ...)  {

  # needed?? 6.5.2014
  # if (is.null(dev.list()))  plot.new()

  grp <- factor(grp)
  if(!add){
    plot(1, xlim = range(pretty((c(from, to)))), ylim = c(0, nlevels(grp) + 1), 
         type = "n", ylab = "", yaxt = "n", ...)
  }
  xleft <- from
  xright <- to
  ytop <- as.numeric(grp) + height/2
  ybottom <- as.numeric(grp) - height/2
  rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45, 
       col = col, border = border, lty = par("lty"), lwd = par("lwd"))
}
