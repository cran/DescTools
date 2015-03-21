WrdPlot <-
function( type="png", append.cr=TRUE, crop=c(0,0,0,0), main = NULL,
                     picscale=100, height=NA, width=NA, res=300, dfact=1.6, wrd = getOption("lastWord") ){

  # png is considered a good choice for export to word (Smith)
  # http://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html
    
  # height, width in cm!
  # scale will be overidden, if height/width defined
  
  
  # Example: WrdPlot(picscale=30)
  #          WrdPlot(width=8)
  
  .CentimetersToPoints <- function(x) x * 28.35
  .PointsToCentimeters <- function(x) x / 28.35
  # http://msdn.microsoft.com/en-us/library/bb214076(v=office.12).aspx
  
  # handle missing height or width values
  if (is.na(width) ){
    if (is.na(height)) {
      width <- 14
      height <- par("pin")[2] / par("pin")[1] * width
    } else {
      width <- par("pin")[1] / par("pin")[2] * height
    }
  } else {
    if (is.na(height) ){
        height <- par("pin")[2] / par("pin")[1] * width
    }
  }
  
  
  # get a [type] tempfilename:
  fn <- paste( tempfile(pattern = "file", tmpdir = tempdir()), ".", type, sep="" )
  # this is a problem for RStudio....
  # savePlot( fn, type=type )
  # png(fn, width=width, height=height, units="cm", res=300 )
  dev.copy(eval(parse(text=type)), fn, width=width*dfact, height=height*dfact, res=res, units="cm")
  d <- dev.off()
  
  # add it to our word report
  res <- wrd[["Selection"]][["InlineShapes"]]$AddPicture( fn, FALSE, TRUE ) 
  wrdDoc <- wrd[["ActiveDocument"]]
  pic <- wrdDoc[["InlineShapes"]]$Item( wrdDoc[["InlineShapes"]][["Count"]] )
  
  pic[["LockAspectRatio"]] <- -1  # = msoTrue
  picfrmt <- pic[["PictureFormat"]]
  picfrmt[["CropBottom"]] <- .CentimetersToPoints(crop[1])
  picfrmt[["CropLeft"]] <- .CentimetersToPoints(crop[2])
  picfrmt[["CropTop"]] <- .CentimetersToPoints(crop[3])
  picfrmt[["CropRight"]] <- .CentimetersToPoints(crop[4])
  
  if( is.na(height) & is.na(width) ){
    # or use the ScaleHeight/ScaleWidth attributes:
    pic[["ScaleHeight"]] <- picscale
    pic[["ScaleWidth"]] <- picscale
  } else {
    # Set new height:
    if( is.na(width) ) width <- height / .PointsToCentimeters( pic[["Height"]] ) * .PointsToCentimeters( pic[["Width"]] )
    if( is.na(height) ) height <- width / .PointsToCentimeters( pic[["Width"]] ) * .PointsToCentimeters( pic[["Height"]] )
    pic[["Height"]] <- .CentimetersToPoints(height)
    pic[["Width"]] <- .CentimetersToPoints(width)
  }
  
  if( append.cr == TRUE ) { wrd[["Selection"]]$TypeText("\n") 
  } else { 
    wrd[["Selection"]]$MoveRight(wdConst$wdCharacter, 1, 0)
  }
    
  if( file.exists(fn) ) { file.remove(fn) }
  
  if(!is.null(main)){
    # insert caption
    sel <- wrd$Selection()  # "Abbildung"
    sel$InsertCaption(Label=wdConst$wdCaptionFigure, Title=main)
    sel$TypeParagraph()
  }
  
  invisible(pic) 
  
}
