Freq <-
function(x, breaks = hist(x, plot = FALSE)$breaks, include.lowest = TRUE,
                 ord = c("level", "desc", "asc", "name"),
                 useNA = c("no", "ifany", "always"), ...){
  
  # check if x is a vector (do not use is.vector())
  if(!(is.atomic(x) || is.list(x))) stop("'x' must be a vector")

  if(inherits(x, "table")){
    tab <- x
    
  } else {
    
    if(is.numeric(x)){
      x <- cut(x, breaks = breaks, include.lowest = include.lowest, 
               ordered_result = TRUE, ...)
    }
    
    tab <- table(x, useNA = useNA)
  }
  
  # how should the table be sorted, by name, level or frq? (NULL means "desc")
  switch(match.arg(ord, c("level", "desc", "asc", "name")), 
           level  = {  }
         , name   = { tab <- tab[rownames(tab)] }
         , asc    = { tab <- sort(tab) }
         , desc   = { tab <- -sort(-tab) } 
  )
  
  ptab <- prop.table(tab)
  names(tab)[is.na(names(tab))] <- "<NA>"
  
  z <- data.frame(level = names(tab), 
                  freq = as.vector(tab[]), perc = as.vector(ptab[]), 
                  cumfreq = cumsum(tab[]), cumperc = cumsum(ptab[]))
  
  rownames(z) <- NULL # enumerate from 1:nrow(z)
  class(z) <- c("Freq", "data.frame")
  return(z)
  
}
