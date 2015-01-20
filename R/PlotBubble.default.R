PlotBubble.default <-
function(x, y, area, col, border = NA, na.rm = FALSE, inches=FALSE, ...) {

  # http://blog.revolutionanalytics.com/2010/11/how-to-make-beautiful-bubble-charts-with-r.html
  
  d.frm <- data.frame(x=x, y=y, area=area, col=col)
  if(na.rm) d.frm <- d.frm[complete.cases(d.frm),]

  xlim <- range(pretty( sqrt(area[c(which.min(d.frm$x), which.max(d.frm$x))] / pi) * c(-1,1) + c(min(d.frm$x),max(d.frm$x)) ))
  ylim <- range(pretty( sqrt(area[c(which.min(d.frm$y), which.max(d.frm$y))] / pi) * c(-1,1) + c(min(d.frm$y),max(d.frm$y)) ))
  
  # make sure we see all the bubbles
  plot(x = x, y = y, xlim=xlim, ylim=ylim, type="n", ...)
  symbols(x=x, y=y, circles=sqrt(area / pi), fg=border, bg=col, inches=inches, add=TRUE)

}
