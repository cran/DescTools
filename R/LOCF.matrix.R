LOCF.matrix <-
function(x){
  apply(x, 2, LOCF)
}
