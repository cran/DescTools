print.Assocs <-
function(x, digits=4, ...){
  out <- apply(round(x, digits), 2, Format, digits=digits)
  out[c(1,2,17), 2:3] <- "      -"
  dimnames(out) <- dimnames(x)
  
  print(data.frame(out), quote=FALSE)
}
