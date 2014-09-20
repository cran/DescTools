print.Assocs <-
function(x, digits=4, ...){
  out <- apply(round(x, digits), 2, FormatFix, after=digits)
  out[c(1,2,17), 2:3] <- "      -"
  
  print(data.frame(out), quote=FALSE)
}
