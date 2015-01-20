BubbleLegend <-
function(x, y=NULL, radius, cols
                         , labels=NULL, cols.lbl = "black" 
                         , width = NULL, xjust = 0, yjust = 1, inset=0, border="black", frame=TRUE
                         , adj=c(0.5,0.5), cex=1.0, bg = NULL, asp = NULL, ...){
  
  # positionierungscode aus legend
  auto <- if(is.character(x)) 
    match.arg(x, c("bottomright", "bottom", "bottomleft", 
                   "left", "topleft", "top", "topright", "right", "center"))
  else NA      
  
  usr <- par("usr")
  if(is.null(width) ) width <- 2*max(radius) * 1.1
  
  if(is.null(asp)) # get aspect ratio from plot  w/h
    asp <- par("pin")[1]/diff(par("usr")[1:2]) / par("pin")[2]/diff(par("usr")[3:4])
  
  height <- width * asp
  
  if (is.na(auto)) {
    left <- x - xjust * width
    top <- y + (1 - yjust) * height
    
  } else {
    inset <- rep(inset, length.out = 2)
    insetx <- inset[1L] * (usr[2L] - usr[1L])
    left <- switch(auto, bottomright = , topright = , right = usr[2L] - 
                     width - insetx, bottomleft = , left = , topleft = usr[1L] + 
                     insetx, bottom = , top = , center = (usr[1L] + usr[2L] - 
                                                            width)/2)
    insety <- inset[2L] * (usr[4L] - usr[3L])
    top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
                    height + insety, topleft = , top = , topright = usr[4L] - 
                    insety, left = , right = , center = (usr[3L] + usr[4L] + 
                                                           height)/2)
  }
  
  xpd <- par(xpd=TRUE); on.exit(par(xpd))
  
  if(!is.na(frame)) 
    rect( xleft=left, ybottom=top-height, xright=left+width, ytop=top, 
          col=bg, border=frame)

  DrawCircle(x = left + width/2, y = (top - height/2) + max(radius) - radius, radius = radius, col=cols, border=border)
  
  if(!is.null(labels)){
    d <- c(0, 2*radius)
    # ylbl <- (top - height/2) + max(radius) - diff(d) /2 + d[-length(d)]
    ylbl <- rev((top - height/2) + max(radius) - Midx(rev(2*radius), incl.zero = TRUE))
    text(x=left + width/2, y=ylbl, labels=labels, adj=adj, cex=cex, col=cols.lbl, ... ) 	
  }
  
}
