print.Freq <-
function(x, digits=3, ...) {
# print.Freq <- function(x, digits = getOption("digits"), ...) {
# print.Freq <- function(x, digits = eval(expression(.Options$digits), sys.frame(sys.parent())), ...) {
  
  # print as data.frame if something was changed
  if(ncol(x) != 5) {
    print.data.frame(x)
  } else {  
    # object x comes as list lacking a as.data.frame option...
    print(data.frame(level=x$level, freq=x$freq, perc=round(x$perc, digits), cumfreq=x$cumfreq, cumperc=round(x$cumperc, digits)))
  }   
}
