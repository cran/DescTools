PlotVenn <-
function(x, col = "transparent", plot = TRUE) {

  # the dimension of the list
  n <- length(x)
  
  if(n>5) stop("Can't plot a Venn diagram with more than 5 sets...")

  # Get Venn counts
  tab <- table(unlist(x), unlist(lapply( 1:length(x), function(i) rep(LETTERS[i], length(x[[i]])))) )
  venntab <- table( apply( tab, 1, function(x) paste(LETTERS[1:n][as.logical(x)], collapse="") ))
  
  if(plot) {
  
    # PlotVenn
    plot( x=c(-7,7), y=c(-7,7), asp=1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", frame.plot = FALSE)
    
    if( n==2) {

      DrawCircle(x=c(2,-2), y=c(0,0), radius=3, col=col)
      xy <- data.frame(x=c(-3,0,3)
                , y=c(0,0,0)
                , set=c("A","B","AB"))
      text(xy$x, xy$y, labels=venntab[xy$set])

      lbl <- data.frame(x=c(-6,6), y=c(2.5,2.5))
      text( lbl$x, lbl$y, label=LETTERS[1:2], cex=2)

    } else if(n==3) {

      DrawCircle(x=c(2,-1,-1), y=c(0,1.73,-1.73), radius=3, col=col)
      xy <- data.frame(x=c(3.50,-1.75,-1.75,1,-2,1,0)
                , y=c(0,3,-3,1.75,0,-1.75,0)
                , set=c("A","B","C","AB","BC","AC","ABC"))
      text(xy$x, xy$y, labels=venntab[xy$set])

      lbl <- data.frame(x=c(6.5,-4.5,-4.5), y=c(0,4.8,-4.8))
      text( lbl$x, lbl$y, label=LETTERS[1:3], cex=2)

    } else if(n==4) {

      DrawEllipse(x=c(0,0,2,-2), y=c(0,0,-2,-2), radius.x=6, radius.y=4, rot=c(1,3)*pi/4, col=col)
      xy <- data.frame(x=c(-6.0,-4.0,-2.2,0.0,2.2,3.9,5.9,4.3,2.7,-3.1,-4.3,-2.6,-0.1,2.7,0.0)
                , y=c(0.3,-2.9,-4.2,-5.7,-4.2,-2.9,0.2,2.3,4.2,4.0,2.3,0.9,-1.6,0.8,3.4)
                , set=c("A","AC","ACD","AD","ABD","BD","D","CD","C","B","AB","ABC","ABCD","BCD","BC"))
      text(xy$x, xy$y, labels=venntab[xy$set])

      lbl <- data.frame(x=c(-8.0,-4.4,4.5,7.7), y=c(1.9,5.4,5.5,2.5))
      text( lbl$x, lbl$y, label=LETTERS[1:4], cex=2)

    } else if(n==5) {

      DrawEllipse(x=c(0,-1.5,-2,0,1), y=c(0,0,-2,-2.5,-1), radius.x=6, radius.y=3, rot=c(1.7,2.8,4.1,5.4,6.6), col=col)
      xy <- data.frame(x=c(4.9,-0.7,-5.9,-4.3,3.1, 3.6,2.4,0.9,-2.3,-3.8,-4.7,-3.9,-1.5,1.2,3.3,  2.6,1.8,1.2,-0.5,-2.7,-3.7,-4.3,-2.6,-0.9,0.9,3.4,  2.1,-2.1,-3.4,-0.9,-0.5   )
                , y=c(0.5,4.5,1.7,-5.5,-6.1,  -1.1,1.8,2.7,2.9,1.5,-1.1,-3.1,-5,-4.7,-3.1,  0.1,2,1.4,2.4,2.2,0.2,-1.6,-3.3,-4.7,-3.8,-2.5,  -2.1,1.5,-1.3,-3.8,-0.8 )
                , set=c("B","A","E","D","C",  "BE","AB","AD","AE","CE","DE","BD","CD","AC","BC"
                        ,"ABE","ABD", "ABDE","ADE","ACE","CDE","BDE","BCD","ACD","ABC","BCE", "ABCE","ACDE","BCDE","ABCD","ABCDE" )
                , frq=0  )
      xy[match(rownames(venntab), xy$set),"frq"] <- venntab
      text(xy$x, xy$y, labels=xy$frq) # labels=xy$set)

      lbl <- data.frame(x=c(1.8,7.6,5.8,-7.5,-7.9), y=c(6.3,-0.8,-7.1,-6.8,3.9))
      text( lbl$x, lbl$y, label=LETTERS[1:5], cex=2)

    }
  } else {
    xy <- NA
  }
  return(list(venntab, xy))
  
}
