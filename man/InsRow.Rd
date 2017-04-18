\name{InsRow, InsCol}
\alias{InsRow}
\alias{InsCol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert Rows or Columns Into a Matrix
%%  ~~function to do ... ~~
}
\description{Insert rows or columns into a matrix.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
InsRow(m, x, i, row.names = NULL)
InsCol(m, x, i, col.names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{matrix for the rows or columns to be inserted
%%     ~~Describe \code{m} here~~
}
  \item{i}{index of the inserted vector
%%     ~~Describe \code{i} here~~
}
  \item{x}{the vector to be inserted
%%     ~~Describe \code{x} here~~
}
  \item{row.names, col.names}{the dimension names for the inserted vector(s)
%%     ~~Describe \code{row.names} here~~
}
}
\details{The vector \code{x} will be recycled to a length of the next multiple of the number of rows (or columns) of the matrix \code{m} and will be inserted such that the first inserted row (column) has the index \code{i}. If the dimnames are given, they will be used no matter if the matrix m has already dimnames defined or not.
%%  ~~ If necessary, more details than the description above ~~
}
\value{the matrix
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{rbind}}, \code{\link{cbind}}
}
\examples{
m <- matrix(runif(25), 5)

InsRow(m, x=1:10, i=3, row.names = c("X","Y"))
InsRow(m, x=1:10, i=3)

InsCol(m, x=1:10, i=3, col.names = c("X","Y"))
InsCol(m, x=1:10, i=3)
}
\keyword{ manip }% __ONLY ONE__ keyword per line
