Month <-
function (x, format = c("num", "abbr", "full"), lang = c("local", "engl"), stringsAsFactors = TRUE) {

  x <- as.Date(x)
  
  switch(match.arg(arg = format, choices = c("num", "abbr", "full")), 
         num = { res <- as.integer(format(x, "%m")) }, 
         abbr = {
           res <- as.integer(format(x, "%m"))
           switch(match.arg(arg = lang, choices = c("local", "engl")), 
             local = {
               # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
               res <- factor(res, levels=1:12, labels=format(ISOdate(2000, 1:12, 1), "%b"))
               },
             engl = {
               res <- factor(res, levels=1:12, labels=month.abb)
             })
           if(!stringsAsFactors) res <- as.character(res)
         }, 
         full = {
           res <- as.integer(format(x, "%m"))
           switch(match.arg(arg = lang, choices = c("local", "engl")), 
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- factor(res, levels=1:12, labels=format(ISOdate(2000, 1:12, 1), "%B"))
                  },
                  engl = {
                    res <- factor(res, levels=1:12, labels=month.name)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         })
  return(res)
}
