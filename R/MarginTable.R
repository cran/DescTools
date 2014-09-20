MarginTable <-
function(tab){
  lst <- lapply(1:length(dim(tab)),
                function(i) PercTable(margin.table(tab, i)))
  names(lst) <- names(dimnames(tab))
  lst
}
