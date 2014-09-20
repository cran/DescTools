print.mtest <-
function (x, digits = 4L, ...) {
  
  cat("\n")
  cat(strwrap(x$method, prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", x$data.name, "\n", sep = "")
  
  out <- character()
  out <- cbind(format(round(x$statistic, 4)), format.pval(x$p.value, digits = digits), 
               format.pval(x$p.value.corr, digits = digits),
               symnum(x$p.value.corr, corr = FALSE, na = FALSE, 
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                      symbols = c("***", "**", "*", ".", " ")))
  colnames(out) <- c("X-squared", "pval", "pval adj", " ")
  rownames(out) <- if(is.null(rownames(x))) 1:length(x$statistic) else rownames(x)
  print.default(out, digits = 3, quote = FALSE, right = TRUE)
  
  cat("\n")
  cat("---\nSignif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1\n\n") 
  invisible(x)
}
