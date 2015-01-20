qRevGumbel <-
function (p, location = 0, scale = 1)
{
  if (!IsNumeric(scale, positive=TRUE))
    stop("\"scale\" must be positive")
  location + scale * log(-log(p))
}
