PlotRCol <-
function(ord=c("hsv","default"), label=c("text","hex","dec")) {
  # plots all named colors:   PlotRCol(lbel="hex") hat noch zuviele Bezeichnungen
  if( ! is.null(dev.list()) ){ 
    curwin <- dev.cur()
    on.exit(dev.set(curwin))
  } 
  
  # this does not work and CRAN does not allow windows()
  # dev.new(width=13, height=7)

  # colors without greys (and grays...)
  cols <- colors()[-grep( pattern="^gr[ea]y", colors())]
  
  # set order
  switch( match.arg( arg=ord, choices=c("hsv","default") )
	  , "default" = { # do nothing 
      }
    , "hsv" = { 
        rgbc <- col2rgb(cols)
        hsvc <- rgb2hsv(rgbc[1,],rgbc[2,],rgbc[3,])
        cols <- cols[ order(hsvc[1,],hsvc[2,],hsvc[3,]) ]
      }
  )

  cols <- c(cols, rep(NA,3) ) # um 3 NULL-Werte erweitern
 
  zeilen <- 38; spalten <- 12 # 660 Farben 
  farben.zahlen <- matrix( 1:spalten ,nrow=zeilen, ncol=spalten, byrow=T ) # Matrix für Punkte

  x_offset <- 0.5 
  x <- farben.zahlen[,1:spalten]  # x-Werte (Zahlen) 
  y <- -rep(1:zeilen, spalten)    # y-Werte (Zahlen) 

  par(mar=c(0,0,0,0), mex=0.001, xaxt="n", yaxt="n", ann=F)
  plot( x, y 
    , pch=22    # Punkttyp Rechteck 
    , cex=2     # Vergrösserung Punkte 
    , bg=cols   # Hintergrundfarben 
    , bty="n"   # keine Box 
    , xlim=c(1,spalten+x_offset) # x-Wertebereich 
  ) 
  switch( match.arg( arg=label, choices=c("text","hex","dec") )
	  , "text" = {
        text( x+0.1, y, cols, adj=0, cex=0.6 ) # Text Farben  
      }
    , "hex" = {     # HEX-Codes
        text( x+0.1, y, adj=0, cex=0.6, 
        c(apply(apply(col2rgb(cols[1:(length(cols)-3)]), 2, sprintf, fmt=" %02X"), 2, paste, collapse=""), rep("",3))
        )  
      }
    , "dec" = {     # decimal RGB-Codes
        text( x+0.1, y, adj=0, cex=0.6, 
        c(apply(apply(col2rgb(cols[1:(length(cols)-3)]), 2, sprintf, fmt=" %03d"), 2, paste, collapse=""), rep("",3))
        )  
      }
  )
}
