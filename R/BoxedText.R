BoxedText <-
function(x, y = NULL, labels = seq_along(x), adj = NULL,
     pos = NULL, offset = 0.5, vfont = NULL,
     cex = 1, txt.col = NULL, font = NULL, srt = 0, xpad = 0.2, ypad=0.2,
     density = NULL, angle = 45,
     col = "white", border = par("fg"), lty = par("lty"), lwd = par("lwd"), ...) {

     
  .BoxedText <- function(x, y = NULL, labels = seq_along(x), adj = NULL,
       pos = NA, offset = 0.5, vfont = NULL,
       cex = 1, txt.col = NULL, font = NULL, srt = 0, xpad = 0.2, ypad=0.2,
       density = NULL, angle = 45,
       col = "white", border = NULL, lty = par("lty"), lwd = par("lwd"), ...) {

    if(is.na(pos)) pos <- NULL   # we have to change default NULL to NA to be able to repeat it
    if(is.na(vfont)) vfont <- NULL   
    
    w <- strwidth(labels, cex=cex, font=font, vfont=vfont)
    h <- strheight(labels, cex=cex, font=font, vfont=vfont)

    if(length(adj) == 1) adj <- c(adj, 0.5)
    
    xl <- x - adj[1] * w - strwidth("M", cex=cex, font=font, vfont=vfont) * xpad
    xr <- xl + w + 2*strwidth("M", cex=cex, font=font, vfont=vfont) * xpad

    yb <- y - adj[2] * h - strheight("M", cex=cex, font=font, vfont=vfont) * ypad
    yt <- yb + h + 2*strheight("M", cex=cex, font=font, vfont=vfont) * ypad

    xy <- Rotate(x=c(xl,xl,xr,xr), y=c(yb,yt,yt,yb), mx=x, my=y, theta=DegToRad(srt))
    polygon(x=xy$x, y=xy$y, col=col, density=density, angle=angle, border=border, lty=lty, lwd=lwd, ...)

    text(x=x, y=y, labels=labels, adj=adj, pos=pos, offset=offset, vfont=vfont, cex=cex, col=txt.col, font=font, srt=srt)
  }
  
  if(is.null(adj)) adj <- c(0.5, 0.5)
  if (is.null(pos)) pos <- NA
  if (is.null(vfont)) vfont <- NA
  if (is.null(txt.col)) txt.col <- par("fg")
  if (is.null(font)) font <- 1
  if (is.null(density)) density <- NA
  
  # recyle arguments:
  #   which parameter has the highest dimension
  # attention: we cannot repeat NULLs but we can repeat NAs, so we swap NULLs to NAs and 
  #            reset them to NULL above
  lst <- list(x=x, y=y, labels=labels, adj=adj, pos=pos, offset=offset, vfont=vfont,
     cex=cex, txt.col=txt.col, font=font, srt=srt, xpad=xpad, ypad=ypad,
     density=density, angle=angle, col=col, border=border, lty=lty, lwd=lwd)
  maxdim <- max(unlist(lapply(lst, length)))
  
  # recycle all params to maxdim
  lgp <- lapply( lst, rep, length.out=maxdim )

  for( i in 1:maxdim){ 
    .BoxedText(
      x=lgp$x[i], y=lgp$y[i], labels=lgp$labels[i], adj=lgp$adj[i], pos=lgp$pos[i], offset=lgp$offset[i]
      , vfont=lgp$vfont[i], cex=lgp$cex[i], txt.col=lgp$txt.col[i], font=lgp$font[i]
      , srt=lgp$srt[i], xpad=lgp$xpad[i], ypad=lgp$ypad[i], density=lgp$density[i]
      , angle=lgp$angle[i], col=lgp$col[i], border=lgp$border[i], lty=lgp$lty[i], lwd=lgp$lwd[i] ) 
  }  
}
