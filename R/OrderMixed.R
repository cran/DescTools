OrderMixed <-
function(x) {
  
  # - Split each each character string into an vector of strings and
  #   numbers
  # - Separately rank numbers and strings
  # - Combine orders so that strings follow numbers
  
  if(length(x)<1)
    return(NULL)
  else if(length(x)==1)
    return(1)
  
  if( is.numeric(x) )
    return( order(x) )
  
  
  delim="\\$\\@\\$"
  
  numeric <- function(x) {
    suppressWarnings( as.numeric(x) )
  }
  
  nonnumeric <- function(x) {
    suppressWarnings( ifelse(is.na(as.numeric(x)), toupper(x), NA) )
  }
  
  x <- as.character(x)
  
  which.nas <- which(is.na(x))
  which.blanks <- which(x=="")
  
  if(length(which.blanks) >0)
    x[ which.blanks ] <- -Inf
  
  if(length(which.nas) >0)
    x[ which.nas ] <- Inf
  
  ####
  # - Convert each character string into an vector containing single
  #   character and  numeric values.
  ####
  
  # find and mark numbers in the form of +1.23e+45.67
  delimited <- gsub("([+-]{0,1}[0-9]+\\.{0,1}[0-9]*([eE][\\+\\-]{0,1}[0-9]+\\.{0,1}[0-9]*){0,1})",
                    paste(delim,"\\1",delim,sep=""), x)
  
  # separate out numbers
  step1 <- strsplit(delimited, delim)
  
  # remove empty elements
  step1 <- lapply( step1, function(x) x[x>""] )
  
  # create numeric version of data
  step1.numeric <- lapply( step1, numeric )
  
  # create non-numeric version of data
  step1.character <- lapply( step1, nonnumeric )
  
  # now transpose so that 1st vector contains 1st element from each
  # original string
  maxelem <- max(sapply(step1, length))
  
  step1.numeric.t <- lapply(1:maxelem,
                            function(i)
                              sapply(step1.numeric,
                                     function(x)x[i])
  )
  
  step1.character.t <- lapply(1:maxelem,
                              function(i)
                                sapply(step1.character,
                                       function(x)x[i])
  )
  
  # now order them
  rank.numeric   <- sapply(step1.numeric.t,rank)
  rank.character <- sapply(step1.character.t,
                           function(x) as.numeric(factor(x)))
  
  # and merge
  rank.numeric[!is.na(rank.character)] <- 0  # mask off string values
  
  rank.character <- t(
    t(rank.character) +
      apply(matrix(rank.numeric),2,max,na.rm=TRUE)
  )
  
  rank.overall <- ifelse(is.na(rank.character),rank.numeric,rank.character)
  
  order.frame <- as.data.frame(rank.overall)
  if(length(which.nas) > 0)
    order.frame[which.nas,] <- Inf
  retval <- do.call("order",order.frame)
  
  return(retval)
  
}
