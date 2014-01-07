XLGetRange <-
function (file = NULL, sheet = NULL, range = NULL, as.data.frame = TRUE, 
                        header = FALSE, stringsAsFactors = FALSE) {

  A1ToZ1S1 <- function(x){
    xlcol <- c( LETTERS 
      , sort(c(outer(LETTERS, LETTERS, paste, sep="" )))
      , sort(c(outer(LETTERS, c(outer(LETTERS, LETTERS, paste, sep="" )), paste, sep="")))
    )[1:16384]

    z1s1 <- function(x) {
      colnr <- match( regmatches(x, regexec("^[[:alpha:]]+", x)), xlcol)
      rownr <- as.numeric(regmatches(x, regexec("[[:digit:]]+$", x)))
      return(c(rownr, colnr))
    }
    
    lapply(unlist(strsplit(toupper(x),":")), z1s1)
  }

  
  # main function
  
  if(is.null(file)){
    xl <- GetCurrXL()
    ws <- xl$ActiveSheet()
    if(is.null(range)) {
      # if there is a selection in XL then use it, if only one cell selected use currentregion
      if(xl$Selection()$Cells()$Count() == 1 ){
        range <- xl$ActiveCell()$CurrentRegion()$Address(FALSE, FALSE)
      } else {
        range <- xl$Selection()$Address(FALSE, FALSE)
        # there might be more than 1 single region, split by ; 
        # (this might be a problem for other locales)
        range <- unlist(strsplit(range, ";"))
      }  
    }  
  } else {  
    xl <- GetNewXL()
    wb <- xl[["Workbooks"]]$Open(file)
    ws <- wb$Sheets(sheet)$select()
  }
  
  lst <- list()
  for(i in 1:length(range)){
    zs <- A1ToZ1S1(range[i])
    rr <- xl$Range(xl$Cells(zs[[1]][1], zs[[1]][2]), xl$Cells(zs[[2]][1], zs[[2]][2]) )
    lst[[i]] <- rr[["Value2"]]
    names(lst)[i] <- range[i]
  }

  if(!is.null(file)) xl$Quit()  # only quit, if a new XL-instance was created before
  
  # replace NULL values by NAs, as NULLs are evil while coercing to data.frame!
  if(as.data.frame){
    for(i in 1:length(lst)){
      for(j in 1:length(lst[[i]])){
        lst[[i]][[j]][unlist(lapply(lst[[i]][[j]], is.null))] <- NA
      }
      xnames <- unlist(lapply(lst[[i]], "[", 1))        # define the names in case header = TRUE
      if(header) lst[[i]] <- lapply(lst[[i]], "[", -1)  # delete the first row
      lst[[i]] <- do.call(data.frame, c(lapply(lst[[i]][], unlist), stringsAsFactors = stringsAsFactors))
      if(header){
        names(lst[[i]]) <- xnames
      } else {
        names(lst[[i]]) <- paste("X", 1:ncol(lst[[i]]), sep="")
      }  
    }
  }
  
  # just return a single object (for instance data.frame) if only one range was supplied
  if(length(lst)==1) lst <- lst[[1]]
  
  return(lst)
}
