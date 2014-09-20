Weekday <-
function (x, format = c("num", "abbr", "full"), lang = c("local", "engl"), stringsAsFactors = TRUE) {

  x <- as.Date(x)
  
  switch(match.arg(arg = format, choices = c("num", "abbr", "full")), 
         num = { res <- ifelse((wd <- as.integer(format(x, "%w"))) == 0, 7, wd) }, 
         abbr = {
           # weekdays in current locale, Sunday : Saturday, format(ISOdate(2000, 1, 2:8), "%A")
           res <- ifelse((wd <- as.integer(format(x, "%w"))) == 0, 7, wd) 
           switch(match.arg(arg = lang, choices = c("local", "engl")), 
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- factor(res, levels=1:7, labels=format(ISOdate(2000, 1, 3:9), "%a"))
                  },
                  engl = {
                    res <- factor(res, levels=1:7, labels=day.abb)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         }, 
         full = {
           # weekdays in current locale, Sunday : Saturday, format(ISOdate(2000, 1, 2:8), "%A")
           res <- ifelse((wd <- as.integer(format(x, "%w"))) == 0, 7, wd) 
           switch(match.arg(arg = lang, choices = c("local", "engl")), 
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- factor(res, levels=1:7, labels=format(ISOdate(2000, 1, 3:9), "%A"))
                  },
                  engl = {
                    res <- factor(res, levels=1:7, labels=day.name)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         })
  return(res)
}
