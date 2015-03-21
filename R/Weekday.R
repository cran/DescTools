Weekday <-
function (x, fmt = c("d", "dd", "ddd"), lang = c("local", "engl"), stringsAsFactors = TRUE) {

  # x <- as.Date(x)
  res <- as.POSIXlt(x)$wday
  res <- replace(res, res==0, 7)
  
  switch(match.arg(arg = fmt, choices = c("d", "dd", "ddd")), 
         d = { res }, 
         dd = {
           # weekdays in current locale, Sunday : Saturday, format(ISOdate(2000, 1, 2:8), "%A")
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
         ddd = {
           # weekdays in current locale, Sunday : Saturday, format(ISOdate(2000, 1, 2:8), "%A")
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
