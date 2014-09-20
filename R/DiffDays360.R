DiffDays360 <-
function(start_d, end_d, method=c("eu","us")){
  
  # source: http://en.wikipedia.org/wiki/360-day_calendar
  start_d <- as.Date(start_d)
  end_d <- as.Date(end_d)
  
  d1 <- Day(start_d)
  m1 <- Month(start_d)
  y1 <- Year(start_d)
  d2 <- Day(end_d)
  m2 <- Month(end_d)
  y2 <- Year(end_d)
  
  method = match.arg(method)
  switch(method,
    "eu" = {
      if(Day(start_d)==31) start_d <- start_d-1
      if(Day(end_d)==31) end_d <- end_d-1
    }
    , "us" ={
      if( (Day(start_d+1)==1 & Month(start_d+1)==3) & 
            (Day(end_d+1)==1 & Month(end_d+1)==3)) d2 <- 30  
      if( d1==31 || 
            (Day(start_d+1)==1 & Month(start_d+1)==3)) {
          d1 <- 30
          if(d2==31) d2 <- 30
      }    
      
    }
  )  
  
  return( (y2-y1)*360 + (m2-m1)*30 + d2-d1)
  
}
