DunnTest.formula <-
function (formula, data, subset, na.action, method = c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none"),
                              out.list = TRUE, ...) {
  
  if (missing(formula) || (length(formula) != 3L)) 
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  if (length(mf) > 2L) 
    stop("'formula' should be of the form response ~ group")
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  y <- do.call("DunnTest", as.list(mf))
  y$data.name <- DNAME
  y
}
