MoveAvg <-
function(x, order, align = c("center","left","right"), 
                    endrule = c("NA", "keep", "constant")){ 

  n <- length(x)
  align   = match.arg(align)

  switch(align,
  "center" = {
      idx <- c(1:(order %/% 2), (n-order %/% 2+1):n)
      idx_const <- c(rep((order %/% 2)+1, order %/% 2), 
                     rep(n-(order %/% 2), order %/% 2))
        
      if(order %% 2 == 1){   # order is odd
        z <- filter(x, rep(1/order, order), sides=2) 
      } else {           # order is even
        z <- filter(x, c(1/(2*order), rep(1/order, order-1), 1/(2*order)), sides=2)
      }   }
  , "right" = {
      idx <- 1:(order-1)
      idx_const <- order
      z <- filter(x, rep(1/order, order), sides=1)
    }
  , "left" = {
      idx <- (n-order+2):n
      idx_const <- n-order+1
      z <- rev(filter(rev(x), rep(1/order, order), sides=1))
  }
  )
  
  endrule <- match.arg(endrule)
  switch(endrule, 
         "NA" =     {}, 
         keep =     {z[idx] <- x[idx]}, 
         constant = {z[idx] <- z[idx_const]})
  
  if(!is.ts(x)) attr(z, "tsp") <- NULL
  class(z) <- class(x)
  return(z)
}
