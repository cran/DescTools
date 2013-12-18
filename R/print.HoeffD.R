print.HoeffD <-
function(x, ...)
{
  cat("D\n")
  print(round(x$D,2))
  if(length(aad <- x$aad)) {
    cat('\navg|F(x,y)-G(x)H(y)|\n')
    print(round(aad,4))
  }
  if(length(mad <- x$maxad)) {
    cat('\nmax|F(x,y)-G(x)H(y)|\n')
    print(round(mad,4))
  }
  n <- x$n
  if(all(n==n[1,1]))
    cat("\nn=",n[1,1],"\n")
  else {
    cat("\nn\n")
    print(x$n)
  }
  
  cat("\nP\n")
  P <- x$P
  P <- ifelse(P<.0001,0,P)
  p <- format(round(P,4))
  p[is.na(P)] <- ""
  print(p, quote=FALSE)
  invisible()
}
