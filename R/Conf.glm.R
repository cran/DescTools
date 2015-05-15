Conf.glm <-
function(x, cutoff = 0.5, ...){
  resp <- model.extract(x$model, "response")
  pred <- levels(resp)[(predict(x)>cutoff)+1]
  Conf(x=pred, reference=resp, ... )
}
