GetNewWrd <-
function(visible = TRUE, template = "Normal", header=FALSE
                      , main="Descriptive report") {
  
  stopifnot(require(RDCOMClient))
  
  # Starts the Word application with wrd as handle
  wrd <- RDCOMClient::COMCreate("Word.Application", existing=FALSE)
  options(lastWord = wrd)
  
  if( visible == TRUE ) wrd[["Visible"]] <- TRUE 
  
  # Create a new document based on template
  # VBA code:
  # Documents.Add Template:= _
  #        "O:\G\GI\_Admin\Administration\09_Templates\newlogo_GI_doc_bericht.dot", _
  #        NewTemplate:=False, DocumentType:=0
  #
  newdoc <- wrd[["Documents"]]$Add(template, FALSE, 0)
  
  # prepare word document, with front page, table of contents, footer ...
  if(header) .WrdPrepRep( wrd=wrd, main=main )
  
  return( invisible( wrd ) )
}
