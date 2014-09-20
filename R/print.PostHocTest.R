print.PostHocTest <-
function (x, digits = getOption("digits"), ...) {
  
  cat(gettextf("\n  Posthoc multiple comparisons of means : %s \n", attr(x, "method")))
  if (!is.na(attr(x, "conf.level"))) 
    cat("    ", format(100 * attr(x, "conf.level"), 2), "% family-wise confidence level\n", 
        sep = "")
  if (attr(x, "ordered")) 
    cat("    factor levels have been ordered\n")
  cat("\nFit: ", deparse(attr(x, "orig.call"), 500L), "\n\n", 
      sep = "")
  xx <- unclass(x)
  
  attr(xx, "orig.call") <- attr(xx, "conf.level") <- 
    attr(xx, "ordered") <-  attr(xx, "method") <- NULL
  
  if(!is.na(attr(x, "conf.level"))) {
    xx <- lapply(xx, as.data.frame)
    for(nm in names(xx)){
      xx[[nm]]$" " <- FormatSig(xx[[nm]]$"pval")  
      xx[[nm]]$"pval" <- format.pval(xx[[nm]]$"pval", digits=2, nsmall=4)
    }
    
    print.default(xx, digits=digits, ...)
    cat("---\nSignif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1\n\n") 
  } else {
    for(nm in names(xx)){
      xx[[nm]][] <- format.pval(xx[[nm]], 2, na.form = "-")
    }
#     attributes(pp) <- attributes(x$p.value)
    print(xx, digits=digits, quote = FALSE, ...)
  }
  
  invisible(x)
}
