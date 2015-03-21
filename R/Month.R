Month <-
function (x, fmt = c("m", "mm", "mmm"), lang = c("local", "engl"), stringsAsFactors = TRUE) {

  res <- as.POSIXlt(x)$mon + 1
  
  switch(match.arg(arg = fmt, choices = c("m", "mm", "mmm")), 
         m = { res }, 
         mm = {
           # res <- as.integer(format(x, "%m"))
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
         mmm = {
           # res <- as.integer(format(x, "%m"))
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
