Large <-
function(x, k = 5, unique = FALSE, na.rm = FALSE){
  if(na.rm) x <- na.omit(x)
  if(unique){
    ux <- unique(x)
    un <- length(ux)
    lst <- lapply(sort(ux, partial=max((un-k+1), 1):un)[max((un-k+1),1):un], function(n) list(val=n, n=length(which(x == n))))
  } else {
    n <- length(x)
    lst <- lapply(sort(x, partial=max((n-k+1),1):n)[max((n-k+1),1):n], function(n) list(val=n, n=length(which(x == n))))
#   http://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
#    lst <- as.vector(unlist(lapply(lst, "[", "val")))
    lst <- do.call("c", sapply(lst, "[", "val") )
    names(lst) <- NULL
  }
  return(lst)
}
