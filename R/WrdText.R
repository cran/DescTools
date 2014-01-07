WrdText <-
function(txt, fontname="Consolas", fontsize=7, bold=FALSE, italic=FALSE, appendCR=TRUE, wrd=getOption("lastWord") ){

  if (!inherits(txt, "character"))  txt <- capture.output(txt)
  
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
  wrdFont[["Bold"]] <- bold
  wrdFont[["Italic"]] <- italic

  wrdSel$TypeText( paste(txt,collapse="\n") )
  if(appendCR) wrdSel$TypeParagraph()
  
  # Restore old font
  wrdFont[["Name"]] <- currfont[["name"]]
  wrdFont[["Size"]] <- currfont[["size"]]
  wrdFont[["Bold"]] <- currfont[["bold"]]
  wrdFont[["Italic"]] <- currfont[["italic"]]
 
  invisible(currfont) 
  
}
