PlotViolin.formula <-
function (formula, data = NULL, ..., subset) 
{
    if (missing(formula) || (length(formula) != 3)) 
        stop("formula missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- NULL
    
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")

    PlotViolin(split(mf[[response]], mf[-response]), ...)
}
