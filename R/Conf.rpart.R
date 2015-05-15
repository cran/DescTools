Conf.rpart <-
function(x, ...){
  # y <- attr(x, "ylevels")
  Conf(x=attr(x,"ylevels")[x$frame$yval[x$where]], reference=attr(x,"ylevels")[x$y], ...)
}
