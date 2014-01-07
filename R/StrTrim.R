StrTrim <-
function(x, pattern=" \t\n", method="both") { 
  
  switch(match.arg(arg = method, choices = c("both", "left", "right")), 
         both =  { gsub( pattern=gettextf("^[%s]+|[%s]+$", pattern, pattern), replacement="", x=x) }, 
         left =  { gsub( pattern=gettextf("^[%s]+",pattern), replacement="", x=x)  }, 
         right = { gsub( pattern=gettextf("[%s]+$",pattern), replacement="", x=x)  }
         )
  
}
