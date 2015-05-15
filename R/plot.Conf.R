plot.Conf <-
function(x, main="Confusion Matrix", ...){
  mosaicplot(t(x$table), shade=TRUE, main=main, col=c("red", "green"), ...)
}
