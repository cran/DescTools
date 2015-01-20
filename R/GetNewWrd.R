GetNewWrd <-
function(visible = TRUE, template = "Normal", header=FALSE
                       , main="Descriptive report") {
  
  # stopifnot(require(RDCOMClient))
  
  if (requireNamespace("RDCOMClient", quietly = FALSE)) {
  
    # Starts the Word application with wrd as handle
    hwnd <- RDCOMClient::COMCreate("Word.Application", existing=FALSE)
    options(lastWord = hwnd)
    
    if( visible == TRUE ) hwnd[["Visible"]] <- TRUE 
    
    # Create a new document based on template
    # VBA code:
    # Documents.Add Template:= _
    #        "O:\G\GI\_Admin\Administration\09_Templates\newlogo_GI_doc_bericht.dot", _
    #        NewTemplate:=False, DocumentType:=0
    #
    newdoc <- hwnd[["Documents"]]$Add(template, FALSE, 0)
    
    # prepare word document, with front page, table of contents, footer ...
    if(header) .WrdPrepRep( wrd=hwnd, main=main )
  
  } else {
    
    if(Sys.info()["sysname"] == "Windows") 
      warning("RDCOMClient is not available. To install it use: install.packages('RDCOMClient', repos = 'http://www.stats.ox.ac.uk/pub/RWin/')")
    else
      warning(gettextf("RDCOMClient is unfortunately not available for %s systems (Windows-only).", Sys.info()["sysname"]))
            
    hwnd <- NULL
  }
  
  invisible( hwnd ) 
}
