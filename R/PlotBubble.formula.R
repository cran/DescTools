PlotBubble.formula <-
function (formula, data = parent.frame(), ..., subset, ylab = varnames[response]) {
  
  m <- match.call(expand.dots = FALSE)
  eframe <- parent.frame()
  md <- eval(m$data, eframe)
  if (is.matrix(md)) 
    m$data <- md <- as.data.frame(data)
  dots <- lapply(m$..., eval, md, eframe)
  nmdots <- names(dots)
  if ("main" %in% nmdots) 
    dots[["main"]] <- enquote(dots[["main"]])
  if ("sub" %in% nmdots) 
    dots[["sub"]] <- enquote(dots[["sub"]])
  if ("xlab" %in% nmdots) 
    dots[["xlab"]] <- enquote(dots[["xlab"]])
#   if ("panel.first" %in% nmdots) 
#     dots[["panel.first"]] <- match.fun(dots[["panel.first"]])
# http://r.789695.n4.nabble.com/panel-first-problem-when-plotting-with-formula-td3546110.html

  m$ylab <- m$... <- NULL 
  subset.expr <- m$subset
  m$subset <- NULL
  m <- as.list(m)
  m[[1L]] <- stats::model.frame.default
  m <- as.call(c(m, list(na.action = NULL)))
  mf <- eval(m, eframe)
  if (!missing(subset)) {
    s <- eval(subset.expr, data, eframe)
    l <- nrow(mf)
    dosub <- function(x) if (length(x) == l) 
      x[s]
    else x
    dots <- lapply(dots, dosub)
    mf <- mf[s, ]
  }
  
#   horizontal <- FALSE
#   if ("horizontal" %in% names(dots)) 
#     horizontal <- dots[["horizontal"]]
  
  response <- attr(attr(mf, "terms"), "response")
  
  if (response) {
    varnames <- names(mf)
    y <- mf[[response]]
    funname <- NULL
    xn <- varnames[-response]
    if (is.object(y)) {
      found <- FALSE
      for (j in class(y)) {
        funname <- paste0("plot.", j)
        if (exists(funname)) {
          found <- TRUE
          break
        }
      }
      if (!found) 
        funname <- NULL
    }
    if (is.null(funname)) 
      funname <- "PlotBubble"

    if (length(xn)) {
      if (!is.null(xlab <- dots[["xlab"]])) 
        dots <- dots[-match("xlab", names(dots))]
      for (i in xn) {
        xl <- if (is.null(xlab)) 
          i
        else xlab
        yl <- ylab
#         if (horizontal && is.factor(mf[[i]])) {
#           yl <- xl
#           xl <- ylab
#         }
        do.call(funname, c(list(mf[[i]], y, ylab = yl, 
                                xlab = xl), dots))
      }
    }
    else do.call(funname, c(list(y, ylab = ylab), dots))
  }
  
  print(c(list(y, ylab = ylab), dots))
  
  invisible()
}
