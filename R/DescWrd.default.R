DescWrd.default <-
function(x, wrd, main = deparse(substitute(x)), ...) {
  
  .plotReset <- function(){
    layout(matrix(1))
    par(
      xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE, 
      ask = FALSE, bg = "white", bty = "o", cex = 1, cex.axis = 1, 
      cex.lab = 1, cex.main = 1.2, cex.sub = 1, col = "black", 
      col.axis = "black", col.lab = "black", col.main = "black", 
      col.sub = "black", crt = 0, err = 0L, family = "", fg = "black", 
      fig = c(0, 1, 0, 1), fin = c(12.8333333333333, 8), font = 1L, 
      font.axis = 1L, font.lab = 1L, font.main = 2L, font.sub = 1L, 
      #      lab = c(5L, 5L, 7L), las = 0L, lend = "round", lheight = 1, 
      lab = c(5L, 5L, 7L), lend = "round", lheight = 1, 
      ljoin = "round", lmitre = 10, lty = "solid", lwd = 1, 
      mai = c(1.36, 1.09333, 1.093333, 0.56), mar = c(5.1, 4.1,4.1, 2.1), 
      mex = 1, mfcol = c(1L, 1L), mfg = c(1L, 1L, 1L, 1L), 
      mfrow = c(1L, 1L), mgp = c(3, 1, 0), mkh = 0.001, new = FALSE, 
      oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), omi = c(0, 0, 0, 0), 
      pch = 1L, pin = c(11.18, 5.54666666666667), 
      plt = c(0.0851948051948052, 0.956363636363636, 0.17, 0.863333333333333), 
      ps = 16L, pty = "m", smo = 1, srt = 0, tck = NA_real_, 
      tcl = -0.5, usr = c(0, 1, 0, 1), xaxp = c(0, 1, 5), xaxs = "r", xaxt = "s", xpd = FALSE, 
      yaxp = c(0, 1, 5), yaxs = "r", yaxt = "s", ylbias = 0.2)
    #     par(
    #       xlog = FALSE, ylog = FALSE, 
    #       mai = c(1.36, 1.09333, 1.093333, 0.56), mar = c(5.1, 4.1,4.1, 2.1), 
    #       mex = 1, mfcol = c(1L, 1L), mfg = c(1L, 1L, 1L, 1L), 
    #       mfrow = c(1L, 1L), 
    #       oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), omi = c(0, 0, 0, 0), 
    #       usr = c(0, 1, 0, 1), xpd = FALSE 
    #       )
  }
  
  
  if(!is.null(main)) WrdCaption(x=main, wrd=wrd)
  
  txt <- capture.output(Desc(x, ...))[-(1:2)]
  
  # insert table
  if(inherits(x, "Date")) {
    # special Date Table... 
    wrd[["ActiveDocument"]][["Tables"]]$Add( wrd[["Selection"]][["Range"]], NumRows=2, NumColumns=2 )
    wrd[["Selection"]]$MoveRight(Unit=wdConst$wdCharacter, Count=2, Extend=wdConst$wdExtend)
    wrd[["Selection"]][["Cells"]]$Merge()
    
    WrdText( txt=txt[1:6], wrd=wrd )
    wrd[["Selection"]]$MoveRight( wdConst$wdCell, 1, 0)
    WrdText( txt=txt[-c(1:6)], wrd=wrd )
    
  } else {  
    if(max(unlist(lapply(txt, nchar))) < 59){  # decide if two rows or 2 columns ist adequate
      wrd[["ActiveDocument"]][["Tables"]]$Add( wrd[["Selection"]][["Range"]], NumRows=1, NumColumns=2 )
    } else {   
      wrd[["ActiveDocument"]][["Tables"]]$Add( wrd[["Selection"]][["Range"]], NumRows=2, NumColumns=1 )
    }
    WrdText( txt=txt, wrd=wrd )
  }
  wrd[["Selection"]]$MoveRight( wdConst$wdCell, 1, 0)
  
  .plotReset()
  PlotDesc(x, main="", ..., wrd=wrd )
  
  wrd[["Selection"]]$EndOf( wdConst$wdTable )
  # get out of tablerange
  wrd[["Selection"]]$MoveRight( wdConst$wdCharacter, 2, 0 )
  wrd[["Selection"]]$TypeParagraph()

  invisible()
  
}
