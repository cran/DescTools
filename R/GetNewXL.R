GetNewXL <-
function( visible = TRUE ) {
  
  if (requireNamespace("RDCOMClient", quietly = FALSE)) {
    
      # Starts the Excel with xl as handle
    hwnd <- RDCOMClient::COMCreate("Excel.Application")
    if( visible == TRUE ) hwnd[["Visible"]] <- TRUE
    
    # Create a new workbook
    newwb <- hwnd[["Workbooks"]]$Add
  
  } else {

    if(Sys.info()["sysname"] == "Windows") 
      warning("RDCOMClient is not available. To install it use: install.packages('RDCOMClient', repos = 'http://www.stats.ox.ac.uk/pub/RWin/')")
    else
      warning(gettextf("RDCOMClient is unfortunately not available for %s systems (Windows-only).", Sys.info()["sysname"]))
        
    hwnd <- NULL
  }
  
  invisible(hwnd)
  
}
