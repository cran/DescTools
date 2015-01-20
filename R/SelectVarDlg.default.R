SelectVarDlg.default <-
function(x, useIndex = FALSE, ...){

    # example: Sel(d.pizza)
    op <- options(useFancyQuotes = FALSE)
    xsel <- select.list(x, multiple = TRUE, graphics = TRUE)
    if(useIndex == TRUE) { 
      xsel <- which(x %in% xsel)
    } else {
      xsel <- dQuote(xsel)
    }
    txt <- paste("c(", paste(xsel, collapse=","),")", sep="")  

    # utils::writeClipboard(txt)
    .writeCB(txt)
    options(op)

    invisible(txt)
}
