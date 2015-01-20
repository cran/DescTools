rRevGumbel <-
function (n, location = 0, scale = 1)
{
  if (!IsNumeric(scale, positive=TRUE, integer.valued=TRUE))
    stop("bad input for argument \"n\"")
  if (!IsNumeric(scale, positive=TRUE))
    stop("\"scale\" must be positive")
  location + scale * log(-log(runif(n)))
}
