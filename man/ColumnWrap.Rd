% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ColumnWrap.R
\name{ColumnWrap}
\alias{ColumnWrap}
\title{Column Wrap}
\usage{
ColumnWrap(x, width = NULL)
}
\arguments{
\item{x}{the matrix with one row}

\item{width}{integer, the width of the columns in characters}
}
\value{
a character matrix
}
\description{
Wraps text in a character matrix so, that it's displayed over more than one
line.
}
\details{
A data.frame containing character columns with long texts is often wrapped
by columns. This can lead to a loss of overview. \code{ColumnWrap()} wraps the
lines within the columns.
}
\examples{

Abstract(d.pizza)

}
\seealso{
\code{\link[=strwrap]{strwrap()}}
}
\author{
Andri Signorell \href{mailto:andri@signorell.net}{andri@signorell.net}
}
\keyword{print}
