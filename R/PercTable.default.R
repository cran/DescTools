PercTable.default <-
function (x, y = NULL, ...) { 
  
  # all dot arguments
  dot.args <- match.call(expand.dots=FALSE)$...
  # the dot arguments which match PercTable.table
  pt.args <- dot.args[names(dot.args) %in% names(formals(PercTable.table))]
  # the dot arguments which DO NOT match PercTable.table
  tab.args <- dot.args[names(dot.args) %nin% names(formals(PercTable.table))]
  
  if(is.null(y)){
    tab <- do.call("table", append(list(x), tab.args) )
  } else {
    tab <- do.call("table", append(list(x, y), tab.args) )
  }
  do.call( "PercTable", append(list(tab=tab), pt.args) )

}
