StrAbbr <-
function(x, minchar=1, method=c("left","fix")){
  
  switch(match.arg(arg = method, choices = c("left", "fix")),
         "left"={
           idx <- rep(minchar, length(x))-1
           for(i in minchar:max(nchar(x))){
             adup <- AllDuplicated(substr(x, 1, i)) 
             idx[adup] <- i
           }
           res <- substr(x, 1, idx+1)
         },
         "fix"={
           i <- 1
           while(sum(duplicated(substr(x, 1, i))) > 0) { i <- i+1 }
           res <- substr(x, 1, pmax(minchar, i))
         }         
  )
  return(res)
}
