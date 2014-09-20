HotellingsT2Test.formula <-
function(formula, na.action = na.fail, ...) {
  
    if(missing(formula)
       || (length(formula) != 3)
       || (length(attr(terms(formula[-2]), "term.labels")) != 1))
      stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2)
      stop("grouping factor must have exactly 2 levels")
    DATA <- split(as.data.frame(mf[[response]]), g)
    names(DATA) <- c("X", "Y")
    y <- do.call("HotellingsT2Test", c(DATA, list(...)))
    y$data.name <- DNAME
    return(y)
  }
