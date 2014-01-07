TextContrastColor <-
function(col, method=c("glynn","sonego")) {

  switch( match.arg( arg=method, choices=c("glynn","sonego") )
          , "glynn" = {
            # efg, Stowers Institute for Medical Research
            # efg's Research Notes:
            #   http://research.stowers-institute.org/efg/R/Color/Chart
            #
            # 6 July 2004.  Modified 23 May 2005.
            
            # For a given col, define a text col that will have good contrast.
            #   Examples:
            #     > GetTextContrastcol("white")
            #     [1] "black"
            #     > GetTextContrastcol("black")
            #     [1] "white"
            #     > GetTextContrastcol("red")
            #     [1] "white"
            #     > GetTextContrastcol("yellow")
            #     [1] "black"
            vx <- rep("white", length(col))
            vx[ apply(col2rgb(col), 2, mean) > 127 ] <- "black"
            
          }
          , "sonego" = {
            # another idea from Paolo Sonego in OneRTipaDay:
            L <- c(0.2, 0.6, 0) %*% col2rgb(col) / 255
            vx <- ifelse(L >= 0.2, "#000060", "#FFFFA0")
          }
  )
  
  return(vx)

}
