DecToBin <-
function (x) {
  # This would be nice, but does not work: (intToBin from R.utils)
  # y <- as.integer(x)
  # class(y) <- "binmode"
  # y <- as.character(y)
  # dim(y) <- dim(x)
  # y
  unlist(sapply(x, function(x) as.integer(paste(rev(as.integer(intToBits(x))), collapse=""))))
}
