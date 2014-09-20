MixColor <-
function (col1, col2, amount1=0.5) {
  
  .mix <- function(col1, col2, amount1=0.5) {
    # calculate mix
    mix <- apply(col2rgb(c(col1, col2), alpha=TRUE), 1, function(x) amount1 * x[1] + (1-amount1) * x[2])
    do.call("rgb", c(as.list(mix), maxColorValue=255))
  }
  
  m <- suppressWarnings(cbind(col1, col2, amount1))
  apply(m, 1, function(x) .mix(col1=x[1], col2=x[2], amount1=as.numeric(x[3])))
  
}
