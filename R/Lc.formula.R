Lc.formula <-
function(formula, data, subset, na.action, ...) {

  # this is taken basically from wilcox.test.formula  
  
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]), 
                                                                  "term.labels")) != 1L)) 
    stop("'formula' missing or incorrect")

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
#   mf$na.action <- substitute(na.action)
#   DNAME <- paste(names(mf), collapse = " by ")
#   
#   DATA <- list(table(mf))
#   do.call("Lc", c(DATA, list(...)))
    drop <- TRUE
#   mf <- model.frame(x, data)
    x <- split(x = mf[,1], f = mf[,2], drop=drop, ...)
    
    res <- lapply(x, FUN = "Lc", ...)
    class(res) <- "Lclist"

  return(res)

}
