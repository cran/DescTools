WrdInsTab <-
function(nrow = 1, ncol = 1, heights = NULL, widths = NULL, wrd = getOption("lastWord")){
  
  .CentimetersToPoints <- function(x) x * 28.35
  
  res <- wrd[["ActiveDocument"]][["Tables"]]$Add(wrd[["Selection"]][["Range"]], 
                                                 NumRows = nrow, NumColumns = ncol)
  if(!is.null(widths)) {
    widths <- rep(widths, length.out=ncol)
    for(i in 1:ncol){
      # set column-widths
      tcol <- res$Columns(i)
      tcol[["Width"]] <- .CentimetersToPoints(widths[i])
    }  
  }
  if(!is.null(heights)) {
    heights <- rep(heights, length.out=nrow)
    for(i in 1:nrow){
      # set row heights
      tcol <- res$Rows(i)
      tcol[["Height"]] <- .CentimetersToPoints(heights[i])
    }  
  }
  invisible(res)
}
