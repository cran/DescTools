PlotArea.formula <-
function (formula, data, subset, na.action=NULL, ...) {

  m <- match.call(expand.dots=FALSE)
  if(is.matrix(eval(m$data,parent.frame())))   m$data <- as.data.frame(data)

  m$... <- NULL
  m[[1]] <- as.name("model.frame")

  if(as.character(formula[[2]]==".")) {
    rhs <- unlist(strsplit(deparse(formula[[3]])," *[:+] *"))
    lhs <- sprintf("cbind(%s)", paste(setdiff(names(data),rhs),collapse=","))
    m[[2]][[2]] <- parse(text=lhs)[[1]]
  }

  mf <- eval(m, parent.frame())
  if(is.matrix(mf[[1]])) {
    lhs <- as.data.frame(mf[[1]])
    names(lhs) <- as.character(m[[2]][[2]])[-1]
    PlotArea.default(cbind(mf[-1],lhs), ...)
  } else {
    PlotArea.default(mf[2:1], ...)
  }
}
