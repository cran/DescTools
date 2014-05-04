WrdSetFont <-
function(fontname = "Consolas", fontsize = 7, bold = FALSE, italic = FALSE, wrd = getOption("lastWord") ) {
  
  wrdSel <- wrd[["Selection"]]
  wrdFont <- wrdSel[["Font"]]
  
  currfont <- list( 
    name = wrdFont[["Name"]] ,
    size = wrdFont[["Size"]] ,
    bold = wrdFont[["Bold"]] ,
    italic = wrdFont[["Italic"]] 
  )  
  
  wrdFont[["Name"]] <- fontname
  wrdFont[["Size"]] <- fontsize
  
  invisible(currfont)
  
}
