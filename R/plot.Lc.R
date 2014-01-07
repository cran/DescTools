plot.Lc <-
function(x, general=FALSE, lwd=2, type="l", xlab="p", ylab="L(p)",
                    main="Lorenz curve", las=1, ...)  {
  if(!general)
    L <- x$L
  else
    L <- x$L.general
  plot(x$p, L, type=type, main=main, lwd=lwd, xlab=xlab, ylab=ylab, xaxs="i",
       yaxs="i", las=las, ...)
  abline(0,max(L))
}
