CutQ <-
function(x, breaks=quantile(x, seq(0, 1, by=0.25), na.rm=TRUE), labels=NULL, na.rm = FALSE, ...){
  
  # old version:
  #  cut(x, breaks=probsile(x, breaks=probs, na.rm = na.rm), include.lowest=TRUE, labels=labels)
  
  # $Id: probscut.R 1431 2010-04-28 17:23:08Z ggrothendieck2 $
  # from gtools
  
  if(na.rm) x <- na.omit(x)
  
  if(is.null(labels)) labels <- gettextf("Q%s", 1:(length(breaks)-1))
  
  # probs <- quantile(x, probs)
  dups <- duplicated(breaks)
  if(any(dups)) {
    
    flag <- x %in% unique(breaks[dups])
    retval <- ifelse(flag, paste("[", as.character(x), "]", sep=''), NA)
    uniqs <- unique(breaks)
    
    # move cut points over a bit...
    reposition <- function(cut) {
      flag <- x>=cut
      if(sum(flag)==0)
        return(cut)
      else
        return(min(x[flag]))
    }
    
    newprobs <- sapply(uniqs, reposition)
    retval[!flag] <- as.character(cut(x[!flag], breaks=newprobs, include.lowest=TRUE,...))
    
    levs <- unique(retval[order(x)]) # ensure factor levels are
    # properly ordered
    retval <- factor(retval, levels=levs)
    
    ## determine open/closed interval ends
    mkpairs <- function(x) # make table of lower, upper
      sapply(x,
             function(y) if(length(y)==2) y[c(2,2)] else y[2:3]
      )
    pairs <- mkpairs(strsplit(levs, '[^0-9+\\.\\-]+'))
    rownames(pairs) <- c("lower.bound","upper.bound")
    colnames(pairs) <- levs
    
    closed.lower <- rep(F,ncol(pairs)) # default lower is open
    closed.upper <- rep(T,ncol(pairs)) # default upper is closed
    closed.lower[1] <- TRUE             # lowest interval is always closed
    
    for(i in 2:ncol(pairs))            # open lower interval if above singlet
      if(pairs[1,i]==pairs[1,i-1] && pairs[1,i]==pairs[2,i-1])
        closed.lower[i] <- FALSE
    
    for(i in 1:(ncol(pairs)-1))        # open upper interval if below singlet
      if(pairs[2,i]==pairs[1,i+1] && pairs[2,i]==pairs[2,i+1])
        closed.upper[i] <- FALSE
    
    levs <- ifelse(pairs[1,]==pairs[2,],
                   pairs[1,],
                   paste(ifelse(closed.lower,"[","("),
                         pairs[1,],
                         ",",
                         pairs[2,],
                         ifelse(closed.upper,"]",")"),
                         sep='')
    )
    levels(retval) <- levs
    
  } else
    retval <- cut( x, breaks, include.lowest=TRUE,  labels=labels, ... )
  
  return(retval)
  
}
