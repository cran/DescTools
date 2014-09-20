HmsToSec <-
function(x) {
  
  hms <- as.character(x)
  op <- FALSE
  if (length(hms) == 1) {
    hms <- c(hms, "00:00:00")
    op <- TRUE  
  }
  DF <- sapply(data.frame(do.call(rbind, strsplit(hms, ":"))), function(x){
    as.numeric(as.character(x))
  })
  out <- DF[, 1] * 3600 + DF[, 2] * 60 + DF[, 3]
  if (op) {
    out <- out[1]
  }
  out
}
