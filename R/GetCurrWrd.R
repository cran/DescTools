GetCurrWrd <-
function() {
  
  # try to get a handle to a running word instance
  # hwnd <- comGetObject("Word.Application")
  
  stopifnot(require(RDCOMClient))
  
  # there's no "get"-function in RDCOMClient, so just create a new here..
  hwnd <- COMCreate("Word.Application", existing=TRUE)
  if(is.null(hwnd)) warning("No running Word application found!")
  
  options(lastWord = hwnd)
  
  return(hwnd)
  
}
