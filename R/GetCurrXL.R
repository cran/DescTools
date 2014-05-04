GetCurrXL <-
function() {
  
  
  stopifnot(require(RDCOMClient))
  
  # try to get a handle to a running XL instance
  # there's no "get"-function in RDCOMClient, so just create a new here..
  hwnd <- COMCreate("Excel.Application", existing=TRUE)
  if(is.null(hwnd)) warning("No running Excel application found!")
  
  options(lastXL = hwnd)
  
  return(hwnd)
  
}
