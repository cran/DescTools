GetAllSubsets <-
function(x, min.n=1, max.n=length(x)){
  # returns a list with all subsets of x
  # CAUTION: this can be heavy for moderate length of x
  lst <- lapply( min.n:max.n, function(i) { 
    m <- combn(x,i) # this is a matrix, split into it's columns
    lapply( seq_len(ncol(m)), function(k) m[,k]) 
  }  )
  # Alternative:
  # lst <- lapply(min.n:max.n, function(i) lapply(apply(combn(x,i),2,list),unlist))
  # flatten the list of lists to one single list
  lst <- split(unlist(lst), rep(1:length(idx <- rapply(lst, length)), idx))
  return(lst)
}
