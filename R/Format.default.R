Format.default <-
function(x, digits = NULL, sci = getOption("scipen")
                   , big.mark="", leading = NULL 
                   , zero.form = NULL, na.form = NULL
                   , fmt = NULL, align = "left", width = NULL, ...){
  
  # getOption("OutDec")
  
  if(is.null(fmt)) fmt <- ""
  
  if(class(x) == "Date"){
    
    # fine format codes
    # http://www.autohotkey.com/docs/commands/FormatTime.htm
    
    formatd <- function(x, fmt) {
      
      pat <- ""
      fpat <- ""
      
      i <- 1
      if(length(grep("\\bd{4}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "dddd", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%A-", sep="")
        i <- i+1
      }  
      if(length(grep("\\bd{3}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "ddd", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%a-", sep="")
        i <- i+1
      }  
      if(length(grep("\\bd{2}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "dd", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%d-", sep="")
        i <- i+1
      }  
      if(length(grep("\\bd{1}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "d", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "0?(.+)-", sep="")
        fpat <- paste(fpat, "%d-", sep="")
        i <- i+1
      }  
      if(length(grep("\\bm{4}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "mmmm", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%B-", sep="")
        i <- i+1
      }  
      if(length(grep("\\bm{3}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "mmm", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%b-", sep="")
        i <- i+1
      }  
      if(length(grep("\\bm{2}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "mm", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%m-", sep="")
        i <- i+1
      }  
      if(length(grep("\\bm{1}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "m", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "0?(.+)-", sep="")
        fpat <- paste(fpat, "%m-", sep="")
        i <- i+1
      }  
      if(length(grep("\\by{4}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "yyyy", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%Y-", sep="")
        i <- i+1
      }  
      if(length(grep("\\by{2}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "yy", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%y-", sep="")
        i <- i+1
      }  
      if(length(grep("\\by{1}\\b", fmt)) > 0) {
        fmt <- gsub(pattern = "y", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "0?(.+)-", sep="")
        fpat <- paste(fpat, "%y-", sep="")
        i <- i+1
      }  
      
      sub(pat, fmt, format(x, fpat))
      
    }
    
    res <- formatd(x, fmt=fmt)
    
  } else if(fmt=="*"){
    breaks <- c(0,0.001,0.01,0.05,0.1,1) 
    labels <- c("***","** ","*  ",".  ","   ")
    if(identical(x, NA)) return(NA)
    # example: FormatSig(c(0.3, 0.08, 0.042, 0.001))
    res <- as.character(sapply(x, cut, breaks=breaks, labels=labels, include.lowest=TRUE))
    
  } else if(fmt=="p"){
    res <- format.pval(x, digits = digits, na.form=na.form)
    
  } else {  
    
    # handle percentages
    perc <- (fmt == "%") 
    if(perc) 
      x <- round(x * 100, ifelse(is.null(digits), 0, digits))
    
    if(is.na(sci)){
      res <- formatC(x, digits = digits, width = width, format = "f", 
                     big.mark=big.mark)
    } else {
      idx <- (((abs(x) > .Machine$double.eps) & (abs(x) <= 10^-sci)) | (abs(x) >= 10^sci))
      res <- as.character(rep(NA, length(x)))
      # use which here instead of res[idx], because of NAs
      res[which(idx)] <- formatC(x[which(idx)], digits = digits, width = width, format = "e", 
                                 big.mark=big.mark)
      res[which(!idx)] <- formatC(x[which(!idx)], digits = digits, width = width, format = "f", 
                                  big.mark=big.mark)
    }
    if(perc) res <- paste(res, "%", sep="")
    
    if(!is.null(leading)){
      if(leading=="drop"){
        # drop leading zeros
        res <- gsub("(?<![0-9])0+\\.", "\\.", res, perl = TRUE)
        
        # alternative:
        # res <- gsub("(-?)[^[:digit:]]0+\\.", "\\.", res)
        
        # old: mind the minus
        # res <- gsub("[^[:digit:]]0+\\.","\\.", res)
        
      } else if(grepl("^[0]*$", leading)){
        # leading contains only zeros, so let's use them as leading zeros
#         old:
#         n <- nchar(leading) - unlist(lapply(lapply(strsplit(res, "\\."), "[", 1), nchar))
        
        lz <- function(x, n){
          # just add a given number of leading zeros
          # split at the .
          z <- strsplit(as.character(x), split=".", fixed = TRUE)
          # left side
          zl <- lapply(z, "[", 1)
          zl <- sapply(zl, function(x) sprintf(paste0("%0", n + (x<0)*1, "i"), as.numeric(x)))
          # right side
          zr <- sapply(z, "[", 2)
          zr <- ifelse(is.na(zr), "", paste(".", zr, sep="")) 
          
          paste(zl, zr, sep="") 
          
        }
        # old: did not handle - correctly
        # res <- StrPad(res, pad = "0", width=nchar(res) + pmax(n, 0), adj="right")
        res <- lz(res, nchar(leading))
      }
    }
    if(!is.null(zero.form)) res[abs(x) < .Machine$double.eps] <- StrPad(zero.form, width = Coalesce(width, 1), 
                                                                        adj = ifelse(align=="dec", "right", align))
    if(!is.null(na.form)) res[is.na(x)] <- StrPad(na.form, width = Coalesce(width, 1), 
                                                  adj = ifelse(align=="dec", "right", align))
    
    switch(match.arg(align, c("left", "right", "center", "dec"))
           , left  = { res <- gsub("^ ","", res) }   # delete all space on the left
           , right = { res <- StrPad(res, width = max(nchar(res)), pad = " ", adj="right")}
           , center= { res <- StrPad(StrTrim(res), width = max(nchar(res)), pad = " ", adj="center" )}  
           , dec   = {  
                       spx <- strsplit(res, "\\.")
                       bef <- lapply(spx, "[", 1)
                       aft <- lapply(spx, "[", 2)
                       
                       res <- paste(
                         replace(StrPad(bef, max(nchar(bef)), " ", adj = "right"), is.na(bef), "") 
                         , replace(StrPad(aft, max(nchar(aft)), " ", adj = "left"), is.na(aft), "") 
                         , sep=".")
                       res[is.na(x)] <- NA
           }
    )
    
  }  
  
  return(res)  
  
  
}
