PostHocTest.matrix <-
function(x, method = c("none","fdr","BH","BY","bonferroni","holm","hochberg","hommel"), 
                               conf.level = 0.95, ...) {
  
  # http://support.sas.com/resources/papers/proceedings14/1544-2014.pdf
  
  # no conf.level supported so far
  conf.level  <- NA
  
  method <- match.arg(method)
  
  pvals <- DescTools::PairApply(t(x), FUN = function(y1, y2) chisq.test(cbind(y1,y2))$p.value, symmetric=TRUE)
  pvals[upper.tri(pvals, diag=TRUE)] <- NA
  
  if(method != "none")
    pvals[] <- p.adjust(pvals, method=method)  
  
  pvals[] <- format.pval(pvals, digits = 2, na.form = "-")
  
  out <- pvals
  out <- out[-1, -ncol(out)]
  
  class(out) <- c("PostHocTest")
  attr(out, "orig.call") <- "table"
  attr(out, "conf.level") <- conf.level
  attr(out, "ordered") <- FALSE
  attr(out, "method") <- method
  attr(out, "method.str") <- gettextf("\n  Posthoc multiple comparisons of means : %s \n", attr(out, "method"))
  
  return(out)
  
}
