Explore <-
function (x) {
  
  # require(manipulate)
  
  .PrepCmd <- function(xvar, yvar, data, col, pch, desc, show){
    
    if(desc){
      if(yvar == "none"){
        s <- gettextf("Desc(%s$%s, plotit=FALSE)", deparse(substitute(data)), xvar)
      } else {
        s <- gettextf("Desc(%s ~ %s, data=%s, plotit=FALSE)", yvar, xvar, deparse(substitute(data)))
      }  
    } else {
      
      if(xvar=="none" & yvar == "none"){
        s <- "Canvas()"
      } else if(yvar == "none"){
        s <- gettextf("PlotDesc(%s$%s, na.rm=TRUE)", deparse(substitute(data)), xvar)
      } else {
        s <- gettextf("plot(%s ~ %s, data=%s", yvar, xvar, deparse(substitute(data)))
        if(!is.na(col)) s <- paste(s, gettextf(", col=as.numeric(%s)", col))
        if(!is.na(pch)) s <- paste(s, gettextf(", pch=as.numeric(%s)", pch))
        s <- paste(s, ")")
      }
      if(show) cat(s, "\n")
    }
    
    return(s)
    
  }
  
  # define the variables here, as the Rcmd check as CRAN will note miss a visible binding:
  #    Explore: no visible binding for global variable 'xvar'
  
  xvar <- character()
  yvar <- character()
  col <- character()
  pch <- character()
  desc <- logical()
  show <- logical()
  
  variables <- c("none", as.list(names(x)))
  snames <- c("none"=NA, as.list(names(x)[!sapply(x, IsNumeric)]))
  
  manipulate::manipulate({
    eval(parse(text = .PrepCmd(xvar, yvar, x, col, pch, desc, show)))
  },  
  yvar = manipulate::picker(variables, initial="none", label = "y-variable     "), 
  xvar = manipulate::picker(variables, initial="none", label = "x-variable     "), 
  col = manipulate::picker(snames, initial="none",     label = "color          "), 
  pch = manipulate::picker(snames, initial="none",     label = "point character"), 
  desc = manipulate::button("Describe"),
  show = manipulate::button("Print command")
  )  
}
