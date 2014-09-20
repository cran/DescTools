GetNewPP <-
function (visible = TRUE, template = "Normal") {
  
  stopifnot(require(RDCOMClient))
  
  pp <- RDCOMClient::COMCreate("PowerPoint.Application")
  if (visible == TRUE) { pp[["Visible"]] <- TRUE }
  
  newpres <- pp[["Presentations"]]$Add(TRUE)
  ppLayoutBlank <- 12    
  newpres[["Slides"]]$Add(1, ppLayoutBlank)    
  options("lastPP" = pp)
  return(invisible(pp))
}
