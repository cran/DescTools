JonckheereTerpstraTest.formula <-
function (formula, data, subset, na.action, ...) {

    if (missing(formula) || (length(formula) != 3L)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    y <- do.call("JonckheereTerpstraTest", as.list(mf))
    y$data.name <- DNAME
    y
}
