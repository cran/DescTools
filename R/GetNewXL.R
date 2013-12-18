GetNewXL <-
function( visible = TRUE ) {

  stopifnot(require(RDCOMClient))
  
  # Starts the Excel with xl as handle
  xl <- COMCreate("Excel.Application")
  if( visible == TRUE ) xl[["Visible"]] <- TRUE
 
  # Create a new workbook
  newwb <- xl[["Workbooks"]]$Add

  return(invisible(xl))
}
