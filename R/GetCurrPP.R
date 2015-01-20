GetCurrPP <-
function() {
  
  #  stopifnot(require(RDCOMClient))
  if (requireNamespace("RDCOMClient", quietly = FALSE)) {
    
    # there's no "get"-function in RDCOMClient, so just create a new here..
    hwnd <- RDCOMClient::COMCreate("PowerPoint.Application", existing=TRUE)
    if(is.null(hwnd)) warning("No running PowerPoint application found!")
    
    options("lastPP" = hwnd)
  
  } else {

    if(Sys.info()["sysname"] == "Windows") 
      warning("RDCOMClient is not available. To install it use: install.packages('RDCOMClient', repos = 'http://www.stats.ox.ac.uk/pub/RWin/')")
    else
      warning(gettextf("RDCOMClient is unfortunately not available for %s systems (Windows-only).", Sys.info()["sysname"]))
        
    hwnd <- NULL
  }

  
  invisible(hwnd)
  
}
