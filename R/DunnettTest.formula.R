DunnettTest.formula <-
function (formula, data, subset, na.action, control = NULL
                                 , conf.level = 0.95, ...) {
  
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
  
  # checkme !!!!!!!!!!!!
  # problem: this does not pass on control and conf.level to .default function!!!

  # old:
  y <- do.call("DunnettTest", as.list(mf))

#   response <- attr(attr(mf, "terms"), "response")
#   g <- factor(mf[[-response]])
#   
#   DATA <- split(as.data.frame(mf[[response]]), g)
#   names(DATA) <- c("x", "g")
#   y <- do.call("DunnettTest", c(DATA, list(...)))
    
  y$data.name <- DNAME
  y
}
