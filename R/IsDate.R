IsDate <-
function(x, what=c('either','both','timeVaries')) {

  what <- match.arg(what)
  cl <- class(x) # was oldClass 22jun03
  if(!length(cl)) return(FALSE)

  dc <- c('POSIXt','POSIXct','dates','times','chron','Date')
  dtc <- c('POSIXt','POSIXct','chron') 
  switch(what,
    either = any(cl %in% dc),
    both = any(cl %in% dtc),
    timeVaries = {
      # original: if('chron' %in% cl || !.R.) { ### chron or S+ timeDate
      if('chron' %in% cl) { # chron ok, but who cares about S+?
        y <- as.numeric(x)
        length(unique(round(y - floor(y),13))) > 1
      } else {
        length(unique(format(x, '%H%M%S'))) > 1 
      }  
    }
  )
    
}
