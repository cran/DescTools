print.PostHocTest <-
function (x, digits = getOption("digits"), ...) {
  
  cat(attr(x, "method.str"))
  if (!is.na(attr(x, "conf.level"))) 
    cat("    ", format(100 * attr(x, "conf.level"), 2), "% family-wise confidence level\n", 
        sep = "")
  if (attr(x, "ordered")) 
    cat("    factor levels have been ordered\n")
  if(!is.language(attr(x, "orig.call"))) 
    cat("\nFit: ", deparse(attr(x, "orig.call"), 500L), "\n\n", sep = "")
  else
    cat("\n")
  xx <- unclass(x)
  
  attr(xx, "orig.call") <- attr(xx, "conf.level") <- 
    attr(xx, "ordered") <-  attr(xx, "method.str") <-  attr(xx, "method") <- NULL
  
  xx["data.name"] <- NULL
  
  if(!is.na(attr(x, "conf.level"))) {
    xx <- lapply(xx, as.data.frame)
    for(nm in names(xx)){
      xx[[nm]]$" " <- Format(xx[[nm]]$"pval", fmt="*")  
      xx[[nm]]$"pval" <- format.pval(xx[[nm]]$"pval", digits=2, nsmall=4)
    }
    
    print.default(xx, digits=digits, ...)
    cat("---\nSignif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1\n") 
  } else {
    for(nm in names(xx)){
      xx[[nm]][] <- format.pval(xx[[nm]], 2, na.form = "-")
    }
#     attributes(pp) <- attributes(x$p.value)
    print(xx, digits=digits, quote = FALSE, ...)
  }
  cat("\n")

  invisible(x)
}
