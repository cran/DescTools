AllDuplicated <-
function(x){
  # returns an index vector of all values involved in ties
  # so !AllDuplicated determines all values in x just appearing once
  duplicated(x, fromLast=FALSE) | duplicated(x, fromLast=TRUE)
}
