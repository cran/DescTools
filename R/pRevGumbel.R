pRevGumbel <-
function (q, location = 0, scale = 1) {
  
  if (!IsNumeric(scale, positive=TRUE))
    stop("\"scale\" must be positive")
  1-exp(-exp((q - location)/scale))
}
