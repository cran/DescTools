HmsToSec <-
function(x) {
  
  hms <- as.character(x)
  z <- sapply(data.frame(do.call(rbind, strsplit(hms, ":"))), 
              function(x) { as.numeric(as.character(x)) })
  z[,1] * 3600 + z[,2] * 60 + z[,3]
}
