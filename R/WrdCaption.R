WrdCaption <-
function(x, stylename = wdConst$wdStyleHeading1, wrd = getOption("lastWord") ) {

  wrdSel <- wrd[["Selection"]]  
  wrdFont <- wrdSel[["Font"]]

  if(is.null(wrdSel)) stop("No running word found!")
  
  currfont <- list( 
    name = wrdFont[["Name"]] ,
    size = wrdFont[["Size"]] ,
    bold = wrdFont[["Bold"]] ,
    italic = wrdFont[["Italic"]] 
  )  

  wrdSel[["Style"]] <- stylename
  wrdSel$TypeText(x)
  wrdSel$TypeParagraph()

  wrdSel[["Style"]] <- "Standard"

  # Restore old font
  wrdFont[["Name"]] <- currfont[["name"]]
  wrdFont[["Size"]] <- currfont[["size"]]
  wrdFont[["Bold"]] <- currfont[["bold"]]
  wrdFont[["Italic"]] <- currfont[["italic"]]
  
}
