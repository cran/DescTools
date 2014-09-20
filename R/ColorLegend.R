ColorLegend <-
function( x, y=NULL, cols=rev(heat.colors(100)), labels=NULL 
  , width=NULL, height=NULL, horiz=FALSE
  , xjust=0, yjust=1, inset=0, border=NA, frame=NA
  , cntrlbl = FALSE 
  , adj=ifelse(horiz,c(0.5,1), c(1,0.5)), cex=1.0, ...){

  # positionierungscode aus legend
  auto <- if (is.character(x)) 
    match.arg(x, c("bottomright", "bottom", "bottomleft", 
        "left", "topleft", "top", "topright", "right", "center"))
  else NA      

  usr <- par("usr")
  if( is.null(width) ) width <- (usr[2L] - usr[1L]) * ifelse(horiz, 0.92, 0.08)
  if( is.null(height) ) height <- (usr[4L] - usr[3L]) * ifelse(horiz, 0.08, 0.92)
  
  if (is.na(auto)) {
    left <- x - xjust * width
    top <- y + (1 - yjust) * height

  } else {
    inset <- rep(inset, length.out = 2)
    insetx <- inset[1L] * (usr[2L] - usr[1L])
    left <- switch(auto, bottomright = , topright = , 
        right = usr[2L] - width - insetx, bottomleft = , 
        left = , topleft = usr[1L] + insetx, bottom = , 
        top = , center = (usr[1L] + usr[2L] - width)/2)
    insety <- inset[2L] * (usr[4L] - usr[3L])
    top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
        height + insety, topleft = , top = , topright = usr[4L] - 
        insety, left = , right = , center = (usr[3L] + 
        usr[4L] + height)/2)
  }
  
  xpd <- par(xpd=TRUE); on.exit(par(xpd))
  
  ncols <- length(cols)	
  nlbls <- length(labels)
  if(horiz) {
    rect( xleft=left, xright=left+width/ncols*seq(ncols,0,-1), ytop=top, ybottom=top-height, 
      col=cols, border=border)
    if(!is.null(labels)){
      if(cntrlbl) xlbl <- left + width/(2*ncols)+(width-width/ncols)/(nlbls-1) * seq(0,nlbls-1,1)
        else xlbl <- left + width/(nlbls-1) * seq(0,nlbls-1,1)
      text(y=top - (height + max(strheight(labels, cex=cex)) * 1.2)
        # Gleiche Korrektur wie im vertikalen Fall
        # , x=x+width/(2*ncols)+(width-width/ncols)/(nlbls-1) * seq(0,nlbls-1,1)
        , x=xlbl, labels=labels, adj=adj, cex=cex, ...) 	
     }   
  } else {
    rect( xleft=left, ybottom=top-height, xright=left+width, ytop=top-height/ncols*seq(0,ncols,1), 
      col=rev(cols), border=border)
    if(!is.null(labels)){
        # Korrektur am 13.6:
        # die grösste und kleinste Beschriftung sollen nicht in der Mitte der Randfarbkästchen liegen, 
        # sondern wirklich am Rand des strips
        # alt: , y=y-height/(2*ncols)- (height- height/ncols)/(nlbls-1)  * seq(0,nlbls-1,1)
        #, y=y-height/(2*ncols)- (height- height/ncols)/(nlbls-1)  * seq(0,nlbls-1,1)
      if(cntrlbl) ylbl <- top - height/(2*ncols) - (height- height/ncols)/(nlbls-1)  * seq(0,nlbls-1,1)
        else ylbl <- top - height/(nlbls-1) * seq(0, nlbls-1, 1)
      text(x=left + width + strwidth("0", cex=cex) + max(strwidth(labels, cex=cex)) * adj[1] 
        , y=ylbl, labels=labels, adj=adj, cex=cex, ... ) 	
    }    
  }
  if(!is.na(frame)) rect( xleft=left, xright=left+width, ytop=top, ybottom=top-height, border=frame)   
}
