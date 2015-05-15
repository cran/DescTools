Conf.randomForest <-
function(x, ...){
  Conf(x=x$predicted, reference=x$y, ... )
}
