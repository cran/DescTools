`%like%` <-
function(x, pattern) { 

    if (!substr(pattern, 1, 1) == "%") { 
      pattern <- paste("^", pattern, sep="")
    } else { 
      pattern <- substr(pattern, 2, nchar(pattern) )   
    }  
    if (!substr(pattern, nchar(pattern), nchar(pattern)) == "%") {
      pattern <- paste(pattern, "$", sep="")
    } else { 
      pattern <- substr(pattern, 1, nchar(pattern)-1 )       
    }  
    
    grepl(pattern = pattern, x = x)
}
