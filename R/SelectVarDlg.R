SelectVarDlg <-
function (x, ...) { 

  # if x is NA then let the user select a data.frame
  # if(missing(x)){
    # x <- select.list( ls(envir=.GlobalEnv)[ lapply( lapply(ls(envir=.GlobalEnv), function(x) gettextf("class(%s)", x)), 
       # function(x) eval(parse(text=x))) == "data.frame" ] , multiple=FALSE)
    # SelectVarDlg.data.frame(x=x, ...)   
  # } else {
    UseMethod("SelectVarDlg")
  
}
