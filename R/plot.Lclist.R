plot.Lclist <-
function(x, col=1, lwd=2, lty=1, main = "Lorenz curve", 
                        xlab="p", ylab="L(p)", ...){
  
  # Recycle arguments
  lgp <- Recycle(x=seq_along(x), col=col, lwd=lwd, lty=lty)
  
  plot(x[[1]], col=lgp$col[1], lwd=lgp$lwd[1], lty=lgp$lty[1], main=main, xlab=xlab, ylab=ylab, ...)
  for(i in 2:length(x)){
    lines(x[[i]], col=lgp$col[i], lwd=lgp$lwd[i], lty=lgp$lty[i])
  }
}
