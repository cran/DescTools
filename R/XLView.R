XLView <-
function (x, col.names = TRUE, row.names = TRUE) {

    fn <- paste(tempfile(pattern = "file", tmpdir = tempdir()), 
        ".csv", sep = "")
    xl <- GetNewXL()
    owb <- xl[["Workbooks"]]
    
    if(!missing(x)){
      write.table(x, file = fn, sep = ";", col.names = col.names, 
        qmethod = "double", row.names = row.names)
      ob <- owb$Open(fn)
      # if row.names are saved there's the first cell in the first line missing
      # I don't actually see, how to correct this besides inserting a cell in XL
      xlToRight <- -4161
      if(row.names) xl$Cells(1, 1)$Insert(Shift=xlToRight)
      xl[["Cells"]][["EntireColumn"]]$AutoFit()

    } else {
      owb$Add()
      awb <- xl[["ActiveWorkbook"]]
      # delete sheets(2,3) without asking, if it's ok
      xl[["DisplayAlerts"]] <- FALSE
      xl$Sheets(c(2,3))$Delete()
      xl[["DisplayAlerts"]] <- TRUE
      awb$SaveAs( Filename=fn, FileFormat=6 )  
    }
    invisible(fn)
}
