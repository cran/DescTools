DescNumNum <-
function(x, y, xname=deparse(substitute(x))
    , yname=deparse(substitute(y))) {

  n <- length(x)
  vn <- sum(complete.cases(x,y))
  digits <- format.info(signif((n-vn)/n*100,3))[2]-2    # hier 3 signifikante Stellen für beide Angaben bestimmen
  cat( "\nSummary: \n",
    "n pairs: ", .fmt(n), 
    ", valid: ", .fmt(vn), " (", round(vn/n*100, digits), "%)",
    ", missings: ", .fmt(n-vn), " (", round((n-vn)/n*100, digits), "%)\n\n"
	, sep="" ) 

  cat(sprintf(
    "\nPearson corr. : %.3f\nSpearman corr.: %.3f\nKendall corr. : %.3f\n"
    , cor(x, y, use="pairwise.complete.obs")
    , cor(x, y, method="spearman", use="pairwise.complete.obs")
    , cor(x, y, method="kendall", use="pairwise.complete.obs")
  ))

  cat("\n")
  
}
