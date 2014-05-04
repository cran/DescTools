PpPlot <-
function( type="png", crop=c(0,0,0,0),
                     picscale=100, x=1, y=1, height=NA, width=NA, res=200, dfact=1.6, pp = getOption("lastPP") ){
  
  # height, width in cm!
  # scale will be overidden, if height/width defined
  
  # Example: PpPlot(picscale=30)
  #          PpPlot(width=8)
  
  .CentimetersToPoints <- function(x) x * 28.35
  .PointsToCentimeters <- function(x) x / 28.35
  # http://msdn.microsoft.com/en-us/library/bb214076(v=office.12).aspx
  
  # get a [type] tempfilename:
  fn <- paste( tempfile(pattern = "file", tmpdir = tempdir()), ".", type, sep="" )
  # this is a problem for RStudio....
  # savePlot( fn, type=type )
  # png(fn, width=width, height=height, units="cm", res=300 )
  dev.copy(png, fn, width=width*dfact, height=height*dfact, res=res, units="cm")
  d <- dev.off()
  
  # add it to our word report
  # slide <- pp[["ActivePresentation"]][["Slides"]]$Item(1)
  slide <- pp$ActiveWindow()$View()$Slide()
  pic <- slide[["Shapes"]]$AddPicture(fn, FALSE, TRUE, x, y)
  
  picfrmt <- pic[["PictureFormat"]]
  picfrmt[["CropBottom"]] <- .CentimetersToPoints(crop[1])
  picfrmt[["CropLeft"]] <- .CentimetersToPoints(crop[2])
  picfrmt[["CropTop"]] <- .CentimetersToPoints(crop[3])
  picfrmt[["CropRight"]] <- .CentimetersToPoints(crop[4])
  
  if( is.na(height) & is.na(width) ){
    # or use the ScaleHeight/ScaleWidth attributes:
    msoTrue <- -1
    msoFalse <- 0
    pic$ScaleHeight(picscale/100, msoTrue)
    pic$ScaleWidth(picscale/100, msoTrue)
    
  } else {
    # Set new height:
    if( is.na(width) ) width <- height / .PointsToCentimeters( pic[["Height"]] ) * .PointsToCentimeters( pic[["Width"]] )
    if( is.na(height) ) height <- width / .PointsToCentimeters( pic[["Width"]] ) * .PointsToCentimeters( pic[["Height"]] )
    pic[["Height"]] <- .CentimetersToPoints(height)
    pic[["Width"]] <- .CentimetersToPoints(width)
  }
  
  if( file.exists(fn) ) { file.remove(fn) }
  
  invisible( pic ) 
  
}
