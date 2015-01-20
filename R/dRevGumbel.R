dRevGumbel <-
function (x, location = 0, scale = 1) { 
  # from VGAM  -- if (is.null(x)) FALSE else ifelse(is.na(x), FALSE, x)
  if (!IsNumeric(scale, positive=TRUE))
      stop("\"scale\" must be positive")
  temp = exp((x - location)/scale)
  temp * exp(-temp)/scale
}
