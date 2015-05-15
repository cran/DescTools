PercTable.table <-
function(tab, row.vars=NULL, col.vars = 2   
                            , digits=3, big.mark = "", pfmt = FALSE, freq=TRUE, rfrq="100", 
                            expected = FALSE, residuals = FALSE, stdres = FALSE, margins = NULL
                            , ...) {
                            
  # labels = c("Sum", "freq", "perc", "p.row", "p.col"),

  # example:
  # tab <- table(d.pizza[,c("driver","operator")])
  # PercTable(tab, rfrq="110", margins=c(1,2))
  
  # create prop tables and format them
  fmt.tab <- function(x, perc, w, pfmt = FALSE) {
    if(pfmt == FALSE) { 
      fmt <- paste("%*.", digits, "f", sep="")
      pfactor <- 1
    } else {
      fmt <- paste("%*.", digits, "f%%", sep="")
      pfactor <- 100
    }  
    if(perc==1) {
      px <- addmargins(prop.table(addmargins(x, 1), 1), 2)
      if(!1 %in% margins) px <- px[,-ncol(px)] 
      if(!2 %in% margins) px <- px[-nrow(px),] 
      class(px) <- "table"
    } else if(perc==2) {
      px <- addmargins(prop.table(addmargins(x, 2), 2), 1)
      if(!1 %in% margins) px <- px[,-ncol(px)] 
      if(!2 %in% margins) px <- px[-nrow(px),] 
      class(px) <- "table"
    } else {    
      px <- prop.table(x)
      if(!is.null(margins)) px <- addmargins(px, if(length(dim(x))==1) {1} else {3 - margins} )  
    }    
    px[] <- sprintf(fmt=fmt, w, px[] * pfactor)
    # set 100% margins to some zero value
    zero <- sprintf("%*s",  w, ".")
    if(perc==1 & (1 %in% margins)) px[, ncol(px)] <- zero
    if(perc==2 & (1 %in% margins)) px[, ncol(px)] <- zero
    if(perc==1 & (2 %in% margins)) px[nrow(px), ] <- zero
    if(perc==2 & (2 %in% margins)) px[nrow(px), ] <- zero
    
    px
  }  
  
  tlst <- list(freq=tab)
  w <- max( c(max(nchar(formatC( tlst[["freq"]][], big.mark=big.mark))), digits + pfmt * 2 + 2)) + 1
  
  # overwrite percents if only 1-dim table
  if(length(dim(tab)) == 1) rfrq <- paste(sum(as.numeric(rfrq) > 0), "00", sep="")
  
  if(unlist(strsplit(rfrq, NULL))[1] == "1")
    tlst[["perc"]] <- fmt.tab(tab, perc=0, w=w, pfmt=pfmt) 
  if(unlist(strsplit(rfrq, NULL))[2] == "1")
    tlst[["p.row"]] <- fmt.tab(tab, perc=1, w=w, pfmt=pfmt) 
  if(unlist(strsplit(rfrq, NULL))[3] == "1")
    tlst[["p.col"]] <- fmt.tab(tab, perc=2, w=w, pfmt=pfmt)
  
  # flip 1 to 2 and 2 to 1 in margins with: 3 - margins
  if(!is.null(margins)) tlst[["freq"]] <- addmargins(tab, if(length(dim(tab))==1) {1} else {3 - margins})
  
  # format tab as.character
  # tlst[["freq"]][] <- sprintf(paste("%",digits+2,".0f",sep=""),tlst[["freq"]][])
  # there's no big.mark for sprintf, so use formatC instead
  tlst[["freq"]][] <- formatC( tlst[["freq"]][], big.mark=big.mark, format="d", width=w )
  if(freq == FALSE) tlst[["freq"]] <- NULL 
  
  # calculate zeroelement in number of spaces and .
  # zero <- gettextf("%s.", paste(rep(" ", max(unlist(lapply(tlst, nchar)))-1), collapse="")) 
  zero <- sprintf("%*s",  w, ".")
  fmt <- paste("%*.", digits, "f", sep="")
  if(expected == TRUE) {
    if(length(dim(tab))==1){
      tlst[["exp"]] <- sprintf(fmt, w, chisq.test(tab)$expected )
    } else {  
      tlst[["exp"]] <- apply(chisq.test(tab)$expected, 2, function(x) sapply(x, function(x) sprintf(fmt, w,x)))
    }
    if(1 %in% margins) tlst[["exp"]] <- cbind(tlst[["exp"]], zero)
    if(2 %in% margins) tlst[["exp"]] <- rbind(tlst[["exp"]], zero)
  }  
  if(residuals == TRUE){ 
    if(length(dim(tab))==1){
      tlst[["res"]] <-  sprintf(fmt, w, chisq.test(tab)$residuals)
    } else {  
      tlst[["res"]] <-  apply(chisq.test(tab)$residuals, 2, function(x) sapply(x, function(x) sprintf(fmt, w,x)))
    }
    if(1 %in% margins) tlst[["res"]] <- cbind(tlst[["res"]], zero)
    if(2 %in% margins) tlst[["res"]] <- rbind(tlst[["res"]], zero)
  }
  if(stdres == TRUE) {
    if(length(dim(tab))==1){
      tlst[["stdres"]] <-  sprintf(fmt, w, chisq.test(tab)$stdres)
    } else {
      tlst[["stdres"]] <-  apply(chisq.test(tab)$stdres, 2, function(x) sapply(x, function(x) sprintf(fmt, w,x)))
    }  
    if(1 %in% margins) tlst[["stdres"]] <- cbind(tlst[["stdres"]], zero)
    if(2 %in% margins) tlst[["stdres"]] <- rbind(tlst[["stdres"]], zero)
  }
  
  if(length(tlst) == 1){ 
    ftab <- ftable(tlst[[1]])
  } else {
    # build a table array, such as to be able to pass it to ftable afterwards...
    if(length(dim(tab))==1){
      ma <- do.call("cbind", tlst) 
    } else {
      ma <- do.call("Mbind", tlst) 
    }
    ftab <- ftable(ma, col.vars=col.vars, row.vars=row.vars)
    
# either at the beginning or no digit on the left 0s are replaced: 
# gsub("([^[:digit:]]|^)0\\."," \\.", c(210.2, 0.232, "a, 0.23"))
    
#    if(!pfmt) ftab <- gsub("([^[:digit:]]|^)0\\."," \\.",ftab)
    if(!pfmt) ftab <- gsub("[^[:digit:]]0\\.","  \\.",ftab)
  }  
  return(ftab)
  
}
