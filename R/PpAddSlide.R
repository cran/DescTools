PpAddSlide <-
function(pp = getOption("lastPP")){

  slides <- pp[["ActivePresentation"]][["Slides"]]
  slides$AddSlide(2, slides$Item(1)[["CustomLayout"]])$Select()
}
