Permn <-
function(x){

#' Determine all permutations of a set.
#'
#' An implementation of the Steinhaus-Johnson-Trotter permutation algorithm.
#' 
#' @param set a set
#' @return a matrix whose rows are the permutations of set
#' @export
#' @examples
#' permutations(1:3)
#' permutations(c('first','second','third'))
#' permutations(c(1,1,3))
#' apply(permutations(letters[1:6]), 1, paste, collapse = '')
# author: David Kahle <david.kahle@gmail.com>
# package: mpoly 
# original function name: permutations

  insert <- function(elem, slot, v){
    n <- length(v)
    if(slot == 1) return( c(elem, v) )
    if(slot == n+1) return( c(v, elem) )      
    c(v[1:(slot-1)], elem, v[slot:n])    
  }

  r <- length(x)
  if(r == 1) return(as.matrix(x))
    
  row2diag <- function(row, direction){
    np1 <- length(row) + 1
    mat <- matrix(nrow = np1, ncol = np1)
    if(direction == -1){
      for(k in 1:np1){
        mat[k,] <- insert(np1, np1-k+1, row)
      }      
    } 
    if(direction == +1){
      for(k in 1:np1){
        mat[k,] <- insert(np1, k, row)
      }
    }
    mat
  } 
  
  stepUp <- function(mat){ # an r x c matrix
    c <- ncol(mat)
    m <- NULL
    for(k in 1:nrow(mat)){
      m <- rbind(m, row2diag(mat[k,],(-1)^k))
    }    
    m    
  }
  
  # iterate stepUp
  out <- matrix(1)
  for(k in 1:(r-1)){
    out <- stepUp(out)
  }
  
  # substitute set values
  for(k in 1:r){
    out[out==k] <- paste('_', x[k], sep = '')
  }
  out <- gsub('_', '', out) # clear PH
  if(is.numeric(x)){
    d <- dim(out)
    out <- as.numeric(out)
    dim(out) <- d
  }
  
  # return
  unique(out)
}
