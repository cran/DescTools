PpAddSlide <-
function(pos = NULL, pp = getOption("lastPP")){
  
  slides <- pp[["ActivePresentation"]][["Slides"]]
  if(is.null(pos)) pos <- slides$Count()+1
  slides$AddSlide(pos, slides$Item(1)[["CustomLayout"]])$Select()

  invisible()
}
