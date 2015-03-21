WrdText <-
function(txt, fixedfont=TRUE, fontname=NULL, 
                    fontsize=NULL, bold=FALSE, italic=FALSE, col=NULL,
                    alignment = c("left","right","center"), spaceBefore=0, spaceAfter=0,
                    lineSpacingRule = wdConst$wdLineSpaceSingle, 
                    appendCR=TRUE, wrd=getOption("lastWord") ){
  
  if(fixedfont){
    fontname <- Coalesce(fontname, getOption("fixedfont", "Consolas")) 
    fontsize <- Coalesce(fontsize, getOption("fixedfontsize", 7))
  }
  
  if (!inherits(txt, "character"))  txt <- .CaptOut(txt)
  
  wrdSel <- wrd[["Selection"]]
  wrdFont <- wrdSel[["Font"]]
  
  currfont <- list( 
    name = wrdFont[["Name"]] ,
    size = wrdFont[["Size"]] ,
    bold = wrdFont[["Bold"]] ,
    italic = wrdFont[["Italic"]],
    color = wrdFont[["Color"]]
  )  
  
  if(!is.null(fontname)) wrdFont[["Name"]] <- fontname
  if(!is.null(fontsize)) wrdFont[["Size"]] <- fontsize
  wrdFont[["Bold"]] <- bold
  wrdFont[["Italic"]] <- italic
  wrdFont[["Color"]] <- Coalesce(col, wdConst$wdColorBlack)
 
  alignment <- switch(match.arg(alignment), 
                      "left"= wdConst$wdAlignParagraphLeft,
                      "right"= wdConst$wdAlignParagraphRight,
                      "center"= wdConst$wdAlignParagraphCenter
  )      
                      
  wrdSel[["ParagraphFormat"]][["Alignment"]] <- alignment
  wrdSel[["ParagraphFormat"]][["SpaceBefore"]]  <- spaceBefore
  wrdSel[["ParagraphFormat"]][["SpaceAfter"]]  <- spaceAfter
  wrdSel[["ParagraphFormat"]][["LineSpacingRule"]] <- lineSpacingRule
  
  wrdSel$TypeText( paste(txt,collapse="\n") )
  if(appendCR) wrdSel$TypeParagraph()
  
  # Restore old font
  wrdFont[["Name"]] <- currfont[["name"]]
  wrdFont[["Size"]] <- currfont[["size"]]
  wrdFont[["Bold"]] <- currfont[["bold"]]
  wrdFont[["Italic"]] <- currfont[["italic"]]
  wrdFont[["Color"]] <- currfont[["color"]]
  
  invisible(currfont) 
  
}
