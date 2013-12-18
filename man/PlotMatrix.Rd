\name{PlotMatrix}
\alias{PlotMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scatterplot Matrix}
\description{
  Plots a scatterplot matrix, for which the variables shown horizontally
  do not necessarily coincide with those shown vertically. If desired,
  the matrix is divided into several blocks such that it fills more than
  1 plot page.
}
\usage{
PlotMatrix(x, y = NULL, data = NULL, panel = l.panel,
  nrows = 0, ncols = nrows, save = TRUE,
  robrange. = FALSE, range. = NULL, pch = NULL, col = 1, 
  reference = 0, ltyref = 3, log = "", xaxs = "r", yaxs = "r",
  xaxmar = NULL, yaxmar = NULL, vnames = NULL,
  main = "", cex.points = NA, cex.lab = 0.7, cex.text = 1.3, cex.title = 1,
  bty = "o", oma = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data for columns (x axis), or formula defining column
    variables. If it is a formula containing a left hand side,
    the left side variables will be used last.}
  \item{y}{data or formula for rows (y axis). Defaults to \code{x}}
  \item{data}{data.frame containing the variables in case \code{x}
    or \code{y} is a formula}
  \item{panel}{a function that generates the marks of the individual
    panels, see Details.
    Defaults essentially to \code{\link{points}} or \code{\link{text}}
    depending on the argument \code{pch}}
  \item{nrows}{number of rows of panels on a page}
  \item{ncols}{number of columns of panels on a page}
  \item{save}{if y is not provided and \code{save==TRUE},
    the first row and the last column are suppressed.}
  \item{robrange.}{if TRUE, robust plot ranges will be used}
  \item{range.}{plot ranges, given as a matrix with 2 rows (min, max)
    and \code{colnames} identifying the variables.}
  \item{pch}{plotting character. A vector of integers, characters or
    strings can also be given for the default panel function}
  \item{col}{color(s) to be used for plotting the observations}
  \item{reference}{coordinates for reference lines to be shown in the
    panels. A named vector can be used to define a value for each or
    any variable.}
  \item{ltyref}{line type for reference lines}
  \item{log}{specifies logarithmic scale of axes. \code{"x"} asks for
    log scale on horizontal axis, \code{"y"}, on vertical axis,
    \code{"xy"}, on both axes.}
  \item{xaxs, yaxs}{styles for x and y axis, see \code{\link{par}}}
  \item{xaxmar, yaxmar}{in which margin should the x- [y-] axis be
    labelled?}
  \item{vnames}{labels for the variables}
  \item{main}{main title for the plot (to be repeated on each plot page)}
  \item{cex.points}{character expansion for showing the observations}
  \item{cex.lab, cex.text}{character expansion for variable labels
    in the margin and in the "diagonal", respectively,
    relative to \code{cex}}
  \item{cex.title}{character expansion for the main title}
  \item{bty}{box type for each panel, see \code{\link{par}}}
  \item{oma}{width of outer margins, ee \code{\link{par}}}
%  \item{mar}{margins for the panels}
%  \item{keepmf}{logical. If TRUE, the graphical settings
%    "mfrow", "mar", "cex", and "mgp" will be maintained, otherwise,
%    the old values will be restored (default).}
%  \item{axes}{logical. If TRUE, axes will be drawn and labels shown
%    (default).} 
  \item{\dots}{further arguments passed to the \code{panel} function}
}
\details{
  If \code{x} or \code{y} is a data.frame, it is converted to a
  numerical matrix.

  The \code{panel} function can be user written. It needs \eqn{>=6} arguments,
  which are given:
  \itemize{
  \item the values of the horizontal variable,
  \item the values of the vertical variable,
  \item the index of the variable shown horizontally, among the
     \code{y} variables,
  \item the index of the variable shown vertically, among the
     \code{x} variables,
  \item argument \code{pch}, and
  \item argument \code{col}
  }

  Since large scatterplot matrices lead to tiny panels, \code{PlotMatrix}
  splits the matrix into blocks of at most \code{nrows} rows and
  \code{ncols} columns. If these numbers are missing, they default to
  \code{nrows=5} and \code{ncols=6} for landscape pages, and to
  \code{nrows=8} and \code{ncols=5} for portrait pages.
}
\value{
  none
}
\author{Werner A. Stahel, ETH Zurich}
\seealso{\code{\link{pairs}}}
\examples{
PlotMatrix(iris[,1:4], main="Iris", pch=as.numeric(iris[,"Species"]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
