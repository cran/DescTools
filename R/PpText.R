PpText <-
function (txt, x=1, y=1, height=50, width=100, fontname = "Calibri", fontsize = 18, bold = FALSE, 
                    italic = FALSE, col = "black", bg = "white", hasFrame = TRUE, pp = getOption("lastPP")) {
  
  msoShapeRectangle <- 1
  
  if (class(txt) != "character") 
    txt <- capture.output(txt)
#  slide <- pp[["ActivePresentation"]][["Slides"]]$Item(1)
  slide <- pp$ActiveWindow()$View()$Slide()
  shape <- slide[["Shapes"]]$AddShape(msoShapeRectangle, x, y, x + width, y+height)
  textbox <- shape[["TextFrame"]]
  textbox[["TextRange"]][["Text"]] <- txt 
  
  tbfont <- textbox[["TextRange"]][["Font"]]
  tbfont[["Name"]] <- fontname
  tbfont[["Size"]] <- fontsize
  tbfont[["Bold"]] <- bold
  tbfont[["Italic"]] <- italic
  tbfont[["Color"]] <- RgbToLong(ColToRgb(col))
  
  textbox[["MarginBottom"]] <- 10
  textbox[["MarginLeft"]] <- 10
  textbox[["MarginRight"]] <- 10
  textbox[["MarginTop"]] <- 10
  
  shp <- shape[["Fill"]][["ForeColor"]]
  shp[["RGB"]] <- RgbToLong(ColToRgb(bg))
  shp <- shape[["Line"]]
  shp[["Visible"]] <- hasFrame
  
  invisible(shape)
  
}
