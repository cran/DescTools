print.ICC <-
function(x, digits = 3, ...){
  cat("\nIntraclass correlation coefficients \n")
  print(x$results, digits=digits)
  cat("\n Number of subjects =", x$ns, "    Number of raters =", x$nr, "\n")
}
