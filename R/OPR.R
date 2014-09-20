OPR <-
function (K, D = NULL, log = FALSE) {

  # Einperiodenrenditen One-period-returns
  if (is.null(D)) 
    D <- rep(0, length(K))
  if (!log){
    res <- (D[-1] + K[-1] - K[-length(K)])/K[-length(K)]
  } else {    
    res <- log((D[-1] + K[-1])/K[-length(K)])
  }
  
  return(res)
  
}
