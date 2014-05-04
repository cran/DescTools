print.ICC <-
function(x, digits = 2, ...){
  cat("Call: ")
  print(x$Call)
  cat("\nIntraclass correlation coefficients \n")
  print(x$results, digits=digits)
  cat("\n Number of subjects =", x$n.obs, "    Number of Judges = ", x$n.judge)
}
