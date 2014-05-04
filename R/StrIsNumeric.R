StrIsNumeric <-
function(x){
  #example: x <- c("123", "-3.141", "foobar123")
  suppressWarnings(!is.na(as.numeric(x)))
}
