print.DunnTest <-
function (x, digits = getOption("digits"), ...) {
  
  cat(gettextf("\n  Dunn's test of multiple comparisons using rank sums : %s \n\n", attr(x, "method")))
  xx <- unclass(x)
  
  if(attr(x, "out.list")==TRUE) {
    xx <- data.frame(x[1])
    xx$" " <- Format(xx$"pval", fmt="*")  
    xx$"pval" <- format.pval(xx$"pval", digits=2, nsmall=4)
    
    print.data.frame(xx, digits=digits, ...)
    cat("---\nSignif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1\n") 
  } else {
    xx[[1]][] <- format.pval(xx[[1]], 2, na.form = "-")
    #     attributes(pp) <- attributes(x$p.value)
    print(xx[[1]], digits=digits, quote = FALSE, ...)
  }
  cat("\n")
  
  invisible(x)
}
