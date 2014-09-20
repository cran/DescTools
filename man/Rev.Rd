\name{Rev}
\alias{Rev}
\alias{Rev.default}
\alias{Rev.matrix}
\alias{Rev.table}
\alias{Rev.data.frame}

\title{Reverse Elements of a Vector or the Rows/Columns of Matrices and Tables
}
\description{\code{Rev} provides a reversed version of its argument. 
It wraps the base function \code{rev} and provides an additional method for data.frames, matrices and tables, such as to 
reverse the order of rows and columns.
}
\usage{
Rev(x, ...)

\method{Rev}{matrix}(x, direction = c("row", "column", "both"), \dots) 

\method{Rev}{table}(x, direction = c("row", "column", "both"), \dots) 

\method{Rev}{data.frame}(x, direction = c("row", "column", "both"), \dots) 

}
\arguments{
  \item{x}{a data.frame, a matrix or a table to be reversed.
}
  \item{direction}{defines the dimensions in which the elements are to be reversed.
  This can be any value out of c(\code{"row"}, \code{"column"}, \code{"both"}), default being \code{"row"}.
  If it is set to \code{"both"} then rows and columns are reversed.
}
  \item{\dots}{the dots are passed to the matrix, resp. table interface.
}
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{rev}}, \code{\link{order}}, \code{\link{sort}}, \code{\link{seq}}
}
\examples{
tab <- matrix( c( 1, 11, 111,
                  2, 22, 222,
                  3, 33, 333), byrow=TRUE, nrow=3)

Rev(tab, direction="row")                  
Rev(tab, direction="column")                  

# abbreviations are accepted
Rev(tab, direction="b")                  
}
\keyword{ manip }
