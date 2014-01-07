Month <-
function (x, format = c("num", "abbr", "full"), stringsAsFactors = TRUE) {
  
  switch(match.arg(arg = format, choices = c("num", "abbr", "full")), 
         num = { res <- as.integer(format(x, "%m")) }, 
         abbr = {
           res <- format(x, "%b")
           # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
           res <- factor(res, levels=format(ISOdate(2000, 1:12, 1), "%b"))
         }, 
         full = {
           res <- format(x, "%B")
           res <- factor(res, levels=format(ISOdate(2000, 1:12, 1), "%B"))
         })
  return(res)
}
