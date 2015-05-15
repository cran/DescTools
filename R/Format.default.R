Format.default <-
function(x, digits = NULL, sci = getOption("scipen")
                   , big.mark="", leading = NULL 
                   , zero.form = NULL, na.form = NULL
                   , fmt = NULL, align = "left", width = NULL, ...){
  
  
#   We accept here a fmt class to be used as user templates
#   example:
#   
#   fmt.int <- structure(list(
#     digits = 5, sci = getOption("scipen"), big.mark = "", 
#     leading = NULL, zero.form = NULL, na.form = NULL, 
#     align = "left", width = NULL, txt="(%s), %s - CHF"), class="fmt"
#   )
#   
#   Format(7845, fmt=fmt.int)
  

  # The defined decimal character:
  # getOption("OutDec")
  
    
  if(is.null(fmt)) fmt <- ""
  if(class(fmt) == "fmt") return(do.call(Format, c(fmt, x=x)))
  
  if(class(x) == "Date"){
    
    # fine format codes
    # http://www.autohotkey.com/docs/commands/FormatTime.htm
    
    formatd <- function(x, fmt) {
      
      pat <- ""
      fpat <- ""
      
      i <- 1
# we used here:
#       if(length(grep("\\bd{4}\\b", fmt)) > 0) 
# which found dddd only as separated string from others (\b ... blank)
# this is not suitable for formats like yyyymmdd
# hence this was changed to d{4}

#      if(length(grep("\\bd{4}\\b", fmt)) > 0) {
      if(length(grep("d{4}", fmt)) > 0) {
        fmt <- gsub(pattern = "dddd", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%A-", sep="")
        i <- i+1
      }  
#      if(length(grep("\\bd{3}\\b", fmt)) > 0) {
      if(length(grep("d{3}", fmt)) > 0) {
        fmt <- gsub(pattern = "ddd", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%a-", sep="")
        i <- i+1
      }  
      if(length(grep("d{2}", fmt)) > 0) {
        fmt <- gsub(pattern = "dd", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%d-", sep="")
        i <- i+1
      }  
      if(length(grep("d{1}", fmt)) > 0) {
        fmt <- gsub(pattern = "d", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "0?(.+)-", sep="")
        fpat <- paste(fpat, "%d-", sep="")
        i <- i+1
      }  
      if(length(grep("m{4}", fmt)) > 0) {
        fmt <- gsub(pattern = "mmmm", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%B-", sep="")
        i <- i+1
      }  
      if(length(grep("m{3}", fmt)) > 0) {
        fmt <- gsub(pattern = "mmm", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%b-", sep="")
        i <- i+1
      }  
      if(length(grep("m{2}", fmt)) > 0) {
        fmt <- gsub(pattern = "mm", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%m-", sep="")
        i <- i+1
      }  
      if(length(grep("m{1}", fmt)) > 0) {
        fmt <- gsub(pattern = "m", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "0?(.+)-", sep="")
        fpat <- paste(fpat, "%m-", sep="")
        i <- i+1
      }  
      if(length(grep("y{4}", fmt)) > 0) {
        fmt <- gsub(pattern = "yyyy", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%Y-", sep="")
        i <- i+1
      }  
      if(length(grep("y{2}", fmt)) > 0) {
        fmt <- gsub(pattern = "yy", replacement = paste("\\\\", i, sep=""), x = fmt)
        pat <- paste(pat, "(.+)-", sep="")
        fpat <- paste(fpat, "%y-", sep="")
        i <- i+1
      }  
      if(length(grep("y{1}", fmt)) > 0) {
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
    
    # format p-values  ********************************************************************
     if(is.null(na.form)) na.form <- "NA"
#     if(is.null(digits)) digits <- max(1L, getOption("digits") - 2L)
#     
#     res <- format.pval(x, digits = digits, na.form=na.form)

      eps <- .Machine$double.eps
      
      if ((has.na <- any(ina <- is.na(x)))) 
        x <- x[!ina]
      r <- character(length(is0 <- x < eps))
      if (any(!is0)) {
        rr <- x <- x[!is0]
        expo <- floor(log10(ifelse(x > 0, x, 1e-50)))
        fixp <- (expo >= -3)
        if (any(fixp)) 
          rr[fixp] <- format(x[fixp], digits = 4)
        if (any(!fixp)) 
          rr[!fixp] <- format(x[!fixp], digits=3, scientific=TRUE)
        r[!is0] <- rr
      }
      if (any(is0)) {
        r[is0] <- gettextf("< %s", format(eps, digits = 2))
      }
      if (has.na) {
        rok <- r
        r <- character(length(ina))
        r[!ina] <- rok
        r[ina] <- na.form
      }
      
      res <- r

  } else if(fmt=="e"){
    res <- formatC(x, digits = digits, width = width, format = "e",
                   big.mark=big.mark)

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
