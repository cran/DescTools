Small <-
function(x, k = 1, unique = FALSE, na.rm = FALSE){
  if(na.rm) x <- na.omit(x)
  if(unique){
    ux <- unique(x)
    un <- length(ux)
    lst <- lapply(sort(ux, partial=1:min(k,un))[1:min(k,un)], function(n) list(val=n, n=length(which(x == n))))
  } else {
    n <- length(x)
    lst <- lapply(sort(x, partial=1:min(k,n))[1:min(k,n)], function(n) list(val=n, n=length(which(x == n))))
#   lst <- as.vector(unlist(lapply(lst, "[", "val")))
#   http://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
    lst <- do.call("c", sapply(lst, "[", "val") )
    names(lst) <- NULL
  }
  return(lst)
}
