WrdGetFont <-
function(wrd = getOption("lastWord") ) {
  # returns the font object list: list(name, size, bold, italic) on the current position

  wrdSel <- wrd[["Selection"]]
  wrdFont <- wrdSel[["Font"]]

  currfont <- list( 
    name = wrdFont[["Name"]] ,
    size = wrdFont[["Size"]] ,
    bold = wrdFont[["Bold"]] ,
    italic = wrdFont[["Italic"]] 
  )  
  return(currfont)
}
