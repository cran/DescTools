Freq <-
function(x, breaks = hist(x, plot = FALSE)$breaks, include.lowest = TRUE
                 , digits = 3
                 , ord = c("level","desc","asc","name")
                 , useNA = c("no", "ifany", "always"), print = TRUE, ... ) {
  
  # coerce x to factor if it's a character vector
  if(inherits(x, "character")) x <- factor(x)
  
  if(!class(x)[1] %in% c("integer","ordered","factor","numeric","Date","logic"))
    stop( gettextf("!Freq! can only handle integer, numeric, factor, Date or logic classes but not class=%s ... ", class(x)) )
  
  switch(class(x)[1]
         , "factor" = { 
           tab <- table(x, useNA=useNA)
           # how should the factors be sorted, by name, level or frq? (NULL means "desc")
           switch( match.arg(ord)
                   , "name" = { tab <- tab[ order(rownames(tab)) ] }
                   , "asc" = { tab <- sort(tab) }
                   , "desc" = { tab <- -sort(-tab) }
                   # "level" = {} is default
           )  
         }
         , "ordered" =  tab <- table(x, useNA=useNA)
         , "logic" =  tab <- table(x, useNA=useNA)
         , tab <- table(cut( x, breaks=breaks, include.lowest=include.lowest, ... ), useNA=useNA)
  )
  ptab <- prop.table(tab)
  # define rowname if NAs are to be included
  names(tab)[is.na(names(tab))] <- "<NA>"
  lst <- data.frame( 
    level=names(tab)
    , freq=unname(tab[])
    , perc=unname(ptab[])
    , cumfreq=cumsum(unname(tab[]))
    , cumperc=cumsum(unname(ptab[]))
  )
  rownames(lst) <- 1:nrow(lst)
  
  # old version:
  # print rounded with 3 digits
  # lst.out <- lst
  # lst.out[,c(3,5)] <- lapply( lst.out[,c(3,5)], round, digits)
  # print(lst.out)
  
  class(lst) <- c("Freq", "data.frame")
  
  if(print) print(lst, digits = digits)
  # return full precision
  invisible(lst)
  
}
