GetCurrPP <-
function() {
  
  # try to get a handle to a running pp instance
  stopifnot(require(RDCOMClient))
  
  # there's no "get"-function in RDCOMClient, so just create a new here..
  hwnd <- RDCOMClient::COMCreate("PowerPoint.Application", existing=TRUE)
  if(is.null(hwnd)) warning("No running PowerPoint application found!")
  
  options("lastPP" = hwnd)
  
  return(hwnd)
  
}
