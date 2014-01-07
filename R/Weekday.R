Weekday <-
function (x, format = c("num", "abbr", "full"), stringsAsFactors = TRUE) {
  
  switch(match.arg(arg = format, choices = c("num", "abbr", "full")), 
         num = { res <- ifelse((wd <- as.integer(format(x, "%w"))) == 0, 7, wd) }, 
         abbr = {
           res <- format(x, "%a")
           # weekdays in current locale, Sunday : Saturday, format(ISOdate(2000, 1, 2:8), "%A")
           if(stringsAsFactors) res <- factor(res, levels=format(ISOdate(2000, 1, 3:9), "%a"))
         }, 
         full = {
           res <- format(x, "%A")
           if(stringsAsFactors) res <- factor(res, levels=format(ISOdate(2000, 1, 3:9), "%A"))
         })
  return(res)
}
