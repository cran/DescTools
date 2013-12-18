PageTest.formula <-
function (formula, data, subset, na.action, ...) {

  if (missing(formula)) 
    stop("formula missing")
  if ((length(formula) != 3L) || (length(formula[[3L]]) != 
                                    3L) || (formula[[3L]][[1L]] != as.name("|")) || (length(formula[[3L]][[2L]]) != 
                                                                                       1L) || (length(formula[[3L]][[3L]]) != 1L)) 
    stop("incorrect specification for 'formula'")
  formula[[3L]][[1L]] <- as.name("+")
  m <- match.call(expand.dots = FALSE)
  m$formula <- formula
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " and ")
  names(mf) <- NULL
  y <- do.call("PageTest", as.list(mf))
  y$data.name <- DNAME
  y
  
}
