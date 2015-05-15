Conf.multinom <-
function(x, ...){
  if(is.null(x$model)) stop("x does not contain model. Run multinom with argument model=TRUE!")
  resp <- model.extract(x$model, "response")
  
  # attention: this will not handle correctly responses defined as dummy codes
  # adapt for that!!  ************************************************************
  # resp <- x$response[,1]
  
  pred <- predict(x, type="class")
  Conf(x=pred, resp, ... )
}
