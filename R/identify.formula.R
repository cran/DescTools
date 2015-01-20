identify.formula <-
function(formula, data, subset, na.action, ...) {
#   mf <- model.frame(x, data)
#   x <- mf[,2]
#   y <- mf[,1]
#   identify(x, y, ...)
  
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]), 
                                                                  "term.labels")) != 1L)) 
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  
  identify(x=mf[[-response]], y=mf[[response]], ...)
  
}
