XLGetWorkbook <-
function (file) {
  
  xlLastCell <- 11
  
  xl <- GetNewXL()
  wb <- xl[["Workbooks"]]$Open(file)
  
  lst <- list()
  for( i in 1:wb[["Sheets"]][["Count"]]){
    ws <- wb[["Sheets", i]]
    ws[["Range", "A1"]][["Select"]]
    rngLast <- xl[["ActiveCell"]][["SpecialCells", xlLastCell]][["Address"]]
    lst[[i]] <- ws[["Range", paste("A1",rngLast, sep=":")]][["Value2"]]
  }
  
  xl$Quit()
  return(lst)
  
}
