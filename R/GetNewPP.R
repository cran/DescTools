GetNewPP <-
function (visible = TRUE, template = "Normal") {
  
  #  stopifnot(require(RDCOMClient))
  if (requireNamespace("RDCOMClient", quietly = FALSE)) {
    
    hwnd <- RDCOMClient::COMCreate("PowerPoint.Application")
    if (visible == TRUE) { hwnd[["Visible"]] <- TRUE }
    
    newpres <- hwnd[["Presentations"]]$Add(TRUE)
    ppLayoutBlank <- 12    
    newpres[["Slides"]]$Add(1, ppLayoutBlank)    
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
