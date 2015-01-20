DenseRank <-
function(x, na.last = TRUE) {
  as.numeric(as.factor(rank(x, na.last)))
}
