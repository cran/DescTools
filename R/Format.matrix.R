Format.matrix <-
function(x, digits = NULL, sci = getOption("scipen")
                           , big.mark="", leading = NULL 
                           , zero.form = NULL, na.form = NULL
                           , fmt = NULL, align = "left", width = NULL, ...){
  x[,] <- Format.default(x=x, digits=digits, sci=sci, big.mark=big.mark, 
                         leading=leading, zero.form=zero.form, na.form=na.form, 
                         fmt=fmt, align=align, width=width, ...)
  return(x)
}
