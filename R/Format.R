Format <-
function(x, digits = NULL, sci = getOption("scipen")
                                     , big.mark="", leading = NULL 
                                     , zero.form = NULL, na.form = NULL
                                     , fmt = NULL, align = "left", width = NULL, ...){
  UseMethod("Format")
}
