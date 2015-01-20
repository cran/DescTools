PlotCirc <-
function(tab, acol = rainbow(sum(dim(tab))), aborder = "darkgrey",
                     rcol = SetAlpha(acol[1:nrow(tab)], 0.5), rborder = "darkgrey", 
                     gap = 5, main = "", labels = NULL, cex.lab = 1.0, 
                     las = 1, adj = NULL, dist = 2){
  
  ribbon <- function( angle1.beg, angle1.end, angle2.beg, angle2.end, 
                      radius1 = 1, radius2 = radius1, col = "blue", 
                      border ="darkgrey" ){
    xy1 <- DescTools::PolToCart( radius1, angle1.beg )
    xy2 <- DescTools::PolToCart( radius2, angle1.end )
    xy3 <- DescTools::PolToCart( radius1, angle2.beg )
    xy4 <- DescTools::PolToCart( radius2, angle2.end )
    
    bez1 <- DescTools::DrawArc(radius.x = radius2, angle.beg = DescTools::CartToPol(xy2$x, xy2$y)$theta, angle.end = DescTools::CartToPol(xy4$x, xy4$y)$theta, plot=FALSE)[[1]]
    bez2 <- DescTools::DrawBezier( x = c(xy4$x, 0, xy3$x), y = c(xy4$y, 0, xy3$y), plot=FALSE )
    bez3 <- DescTools::DrawArc(radius.x = radius1, angle.beg=DescTools::CartToPol(xy3$x, xy3$y)$theta, angle.end=DescTools::CartToPol(xy1$x, xy1$y)$theta, plot=FALSE )[[1]]
    bez4 <- DescTools::DrawBezier( x = c(xy1$x, 0, xy2$x), y = c(xy1$y, 0, xy2$y), plot=FALSE )
    
    polygon( x=c(bez1$x, bez2$x, bez3$x, bez4$x), 
             y=c(bez1$y, bez2$y, bez3$y, bez4$y), col=col, border=border)
  }
  
  n <- sum(tab)
  ncol <- ncol(tab)
  nrow <- nrow(tab)
  d <- DegToRad(gap)    # the gap between the sectors in radiant
  
  acol <- rep(acol, length.out = ncol+nrow)
  rcol <- rep(rcol, length.out = nrow)
  aborder <- rep(aborder, length.out = ncol+nrow)
  rborder <- rep(rborder, length.out = nrow)
  
  mpts.left <- c(0, cumsum(as.vector(rbind(rev(apply(tab, 2, sum))/ n * (pi - ncol * d), d))))
  mpts.right <- cumsum(as.vector(rbind(rev(apply(tab, 1, sum))/ n * (pi - nrow * d), d)))
  mpts <- c(mpts.left, mpts.right + pi) + pi/2 + d/2
  
  DescTools::Canvas(10, main=main, xpd=TRUE)
  DescTools::DrawAnnulusSector(x=0, y=0, radius.in=9.5, radius.out=10, 
                    angle.beg=mpts[seq_along(mpts) %% 2 == 1], angle.end=mpts[seq_along(mpts) %% 2 == 0], 
                    col=acol, border=aborder)
  
  if(is.null(labels)) labels <- rev(c(rownames(tab), colnames(tab)))
  
  ttab <- rbind(Rev(tab, direction="column") / n * (pi - ncol * d), d)
  pts.left <- (c(0, cumsum(as.vector(ttab)))) 
  
  ttab <- rbind(Rev(t(tab), direction="column")/ n * (pi - nrow * d), d)
  pts.right <- (c( cumsum(as.vector(ttab)))) + pi 
  
  pts <- c(pts.left, pts.right) + pi/2 + d/2
  dpt <- data.frame(from=pts[-length(pts)], to=pts[-1])
  
  for( i in 1:ncol) {
    for( j in 1:nrow) {
      lang <- dpt[(i-1)*(nrow+1)+j,]
      rang <- Rev(dpt[-nrow(dpt),])[(j-1)*(ncol+1) + i,]
      ribbon( angle1.beg=rang[,2], angle1.end=lang[,1], angle2.beg=rang[,1], angle2.end=lang[,2], 
              radius1 = 10, radius2 = 9, col = rcol[j], border = rborder[j])
    }}

  out <- DescTools::PolToCart(r = 10 + dist, theta=filter(mpts, rep(1/2,2))[seq(1,(nrow+ncol)*2, by=2)])
  
  if(las == 2){
    if(is.null(adj)) adj <- c(rep(1, nrow), rep(0,ncol))
    adj <- rep(adj, length_out=length(labels))
    sapply(seq_along(labels), 
           function(i) text(out$x[i], out$y[i], labels=labels[i], cex=cex.lab, 
                            srt=DescTools::RadToDeg(atan(out$y[i]/out$x[i])), adj=adj[i]))
  } else {
    text(out, labels=labels, cex=cex.lab, srt=ifelse(las==3, 90, 0), adj=adj)
  }
    
  invisible(out)
  
}
