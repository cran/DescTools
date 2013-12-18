PlotTreemap <-
function(x, grp=NULL, labels=NULL, cex=1.0, text.col="black", col=rainbow(length(x)),
                        labels.grp=NULL, cex.grp=3, text.col.grp="black", border.grp="grey50", 
                        lwd.grp=5, main="") {
  
  SqMap <- function(x) {
    
    .sqmap <- function(z, x0 = 0, y0 = 0, x1 = 1, y1 = 1, lst=list()) {
      
      cz <- cumsum(z$area)/sum(z$area)
      n <- which.min(abs(log(max(x1/y1, y1/x1) * sum(z$area) * ((cz^2)/z$area))))
      more <- n < length(z$area)
      a <- c(0, cz[1:n])/cz[n]
      if (y1 > x1) {
        lst <- list( data.frame(idx=z$idx[1:n], 
                                x0=x0 + x1 * a[1:(length(a) - 1)], 
                                y0=rep(y0, n), x1=x0 + x1 * a[-1], y1=rep(y0 + y1 * cz[n], n)))
        if (more) {
          lst <- append(lst, Recall(z[-(1:n), ], x0, y0 + y1 * cz[n], x1, y1 * (1 - cz[n]), lst))
        }
      } else {
        lst <- list( data.frame(idx=z$idx[1:n], 
                                x0=rep(x0, n), y0=y0 + y1 * a[1:(length(a) - 1)], 
                                x1=rep(x0 + x1 * cz[n], n), y1=y0 + y1 * a[-1]))
        if (more) {
          lst <- append(lst, Recall(z[-(1:n), ], x0 + x1 * cz[n], y0, x1 * (1 - cz[n]), y1, lst))
        }
      }
      lst
    }
    
    # z <- data.frame(idx=seq_along(z), area=z)
    if(is.null(names(x))) names(x) <- seq_along(x)
    x <- data.frame(idx=names(x), area=x)
    res <- do.call(rbind, .sqmap(x))
    rownames(res) <- x$idx
    return(res[,-1])
    
  }
  
  
  PlotSqMap <- function(z, col = NULL, border=NULL, lwd=par("lwd"), add=FALSE){
    if(is.null(col)) col <- as.character(z$col)
    # plot squarified treemap
    if(!add) Canvas(c(0,1), xpd=TRUE)
    for(i in 1:nrow(z)){
      rect(xleft=z[i,]$x0, ybottom=z[i,]$y0, xright=z[i,]$x1, ytop=z[i,]$y1, 
           col=col[i], border=border, lwd=lwd)
    }
  }
  
  
  if(is.null(grp)) grp <- rep(1, length(x))
  if(is.null(labels)) labels <- names(x)
  
  # we need to sort the stuff
  ord <- order(grp, -x)
  x <- x[ord]
  grp <- grp[ord]
  labels <- labels[ord]
  col <- col[ord]
  
  
  # get the groups rects first
  zg <- SqMap(Sort(tapply(x, grp, sum), decreasing=TRUE))
  # the transformation information: x0 translation, xs stretching
  tm <- cbind(zg[,1:2], xs=zg$x1 - zg$x0, ys=zg$y1 - zg$y0) 
  gmidpt <- data.frame(x=apply(zg[,c("x0","x1")], 1, mean),
                       y=apply(zg[,c("y0","y1")], 1, mean))  
  
  if(is.null(labels.grp)) 
    if(nrow(zg)>1) { 
      labels.grp <- rownames(zg)
    } else {
      labels.grp <- NA
    }  
  
  Canvas(c(0,1), xpd=TRUE, asp=NA, main=main)
  
  res <- list()
  
  for( i in 1:nrow(zg)){
    
    # get the group index
    idx <- grp == rownames(zg)[i]
    xg.rect <- SqMap(Sort(x[idx], decreasing=TRUE))
    
    # transform
    xg.rect[,c(1,3)] <- xg.rect[,c(1,3)] * tm[i,"xs"] + tm[i,"x0"]
    xg.rect[,c(2,4)] <- xg.rect[,c(2,4)] * tm[i,"ys"] + tm[i,"y0"]
    
    PlotSqMap(xg.rect, col=col[idx], add=TRUE)
    
    res[[i]] <- list(grp=gmidpt[i,], 
                     child= cbind(x=apply(xg.rect[,c("x0","x1")], 1, mean), 
                                  y=apply(xg.rect[,c("y0","y1")], 1, mean)))
    
    text( x=apply(xg.rect[,c("x0","x1")], 1, mean), 
          y=apply(xg.rect[,c("y0","y1")], 1, mean),
          labels=labels[idx], cex=cex, col=text.col )
  }
  
  names(res) <- rownames(zg)
  
  PlotSqMap(zg, col=NA, add=TRUE, border=border.grp, lwd=lwd.grp)
  
  text( x=apply(zg[,c("x0","x1")], 1, mean), 
        y=apply(zg[,c("y0","y1")], 1, mean),
        labels=labels.grp, cex=cex.grp, col=text.col.grp)
  
  invisible(res)
  
}
