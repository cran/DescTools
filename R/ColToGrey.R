ColToGrey <-
function(col){
  rgb <- col2rgb(col)
  g <- rbind( c(0.3, 0.59, 0.11) ) %*% rgb
  rgb(g, g, g, maxColorValue=255)
}
