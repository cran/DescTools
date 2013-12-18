GiniSimpson <-
function(x, na.rm = FALSE) {
  
  
  # referenz:   Sachs, Angewandte Statistik, S. 57
  
  # example:
  # x <- as.table(c(69,17,7,62))
  # rownames(x) <- c("A","B","AB","0")
  # GiniSimpson(x)
  
  if(na.rm) x <- na.omit(x)
  
  x <- as.table(x)
  ptab <- prop.table(x)
  return(sum(ptab*(1-ptab)))
}
