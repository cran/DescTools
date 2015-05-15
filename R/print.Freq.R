print.Freq <-
function(x, digits=3, ...) {

  # print as data.frame if something was changed
  if(ncol(x) != 5) {
    print.data.frame(x)
  } else {  
    # object x comes as list lacking an as.data.frame option...
    print(data.frame(level=x$level, freq=x$freq, perc=round(x$perc, digits), 
                     cumfreq=x$cumfreq, cumperc=round(x$cumperc, digits)))
  }   
}
