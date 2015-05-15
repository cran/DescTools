Conf.svm <-
function(x, ...){
  Conf(x=predict(x), reference=model.frame(x)[,1], ... )
}
