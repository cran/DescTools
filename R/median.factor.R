median.factor <-
function(x, na.rm = FALSE) {

  # Answered by Hong Ooi on 2011-10-28T00:37:08-04:00
  # http://www.rqna.net/qna/nuiukm-idiomatic-method-of-finding-the-median-of-an-ordinal-in-r.html

  if(is.ordered(x)) return(NA)
  
  if(na.rm) x <- na.omit(x)
  if(any(is.na(x))) return(NA)
    
  levs <- levels(x)
  m <- median(as.integer(x), na.rm = na.rm)
  if(floor(m) != m)
  {
    warning("Median is between two values; using the first one")
    m <- floor(m)
  }
  ordered(m, labels = levs, levels = seq_along(levs))
}
