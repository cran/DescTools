MoveAvg <-
function(x, order, align = c("center","left","right")){ 

  align   = match.arg(align)
  switch(align,
  "center" = {
      if(order %% 2 == 1){   # order is odd
        filter(x, rep(1/order, order), sides=2)
      } else {           # order is even
        filter(x, c(1/(2*order), rep(1/order, order-1), 1/(2*order)), sides=2)
      }   }
  , "right" = {
      filter(x, rep(1/order, order), sides=1)
    }
  , "left" = {
      rev(filter(rev(x), rep(1/order, order), sides=1))
  }
  )
}
