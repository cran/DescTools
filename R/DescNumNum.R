DescNumNum <-
function(x, y, xname=deparse(substitute(x))
    , yname=deparse(substitute(y)), plotit=getOption("plotit", FALSE)) {

  n <- length(x)
  vn <- sum(complete.cases(x,y))
  digits <- format.info(signif((n-vn)/n*100,3))[2]-2    # hier 3 signifikante Stellen fuer beide Angaben bestimmen
  cat( "\nSummary: \n",
    "n pairs: ", .fmt(n), 
    ", valid: ", .fmt(vn), " (", round(vn/n*100, digits), "%)",
    ", missings: ", .fmt(n-vn), " (", round((n-vn)/n*100, digits), "%)\n\n"
	, sep="" ) 
  
  cat(sprintf(
    "\nPearson corr. : %s\nSpearman corr.: %s\nKendall corr. : %s\n"
    , Format(cor(x, y, use="pairwise.complete.obs"), digits=3)
    , Format(cor(x, y, method="spearman", use="pairwise.complete.obs"), digits=3)
    , if(n < 5000){
        Format(cor(x, y, method="kendall", use="pairwise.complete.obs"), digits=3)
      } else {
        "(sample too large)"
      }  
  ))

  cat("\n")
  
  if(plotit){
    d.frm <- data.frame(x=x, y=y)
#     names(d.frm) <-  c(xname, yname)
#     PlotDescNumNum( form1=formula(gettextf("%s ~ %s", xname, yname)),
#                     form2=formula(gettextf("%s ~ %s", yname, xname)), data=d.frm, 
#                     main = gettextf("%s ~ %s", yname, xname) )
    PlotDescNumNum( form1=formula("y ~ x"),
                    form2=formula("x ~ y"), data=d.frm, 
                    main = gettextf("%s ~ %s", yname, xname), xlab = xname, ylab = yname )
  }
  invisible()
  
}
