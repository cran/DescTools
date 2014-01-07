\name{Coalesce}
\alias{Coalesce}

\title{Return the First Element Not Being NA
}
\description{Return the first element of a vector, not being NA. 
}
\usage{
Coalesce(\dots, method = c("is.na", "is.finite"))
}
\arguments{
  \item{\dots}{the elements to be evaluated. This can either be a single vector, several 
  vectors of same length, a matrix, a data.frame or a list of vectors (of same length). See examples.}
  \item{method}{one out of \code{"is.na"} (default) or \code{"is.finite"}. The \code{"is.na"} option allows \code{Inf} values to 
   be in the result, the second one eliminates them. 
}
}
\details{
The evaluation will be rowwise. The first element of the result 
is the first non NA element of the first elements of all the arguments, the second element of 
the result is the one of the second elements of all the arguments and so on. \cr
Shorter inputs (of non-zero length) are NOT recycled.\cr
The idea is borrowed from SQL. Might sometimes be useful when preparing data in R instead of in SQL. 
}
\value{return a single vector of the first non NA element(s) of the given data structure.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{is.na}}, \code{\link{is.finite}}
}


\examples{

Coalesce(c(NA, NA, NA, 5, 3))
Coalesce(c(NA, NULL, "a"))
Coalesce(NULL, 5, 3)

d.frm <- data.frame(matrix(c(
  1,2,NA,4,
  NA,NA,3,1,
  NaN,2,3,1,
  NA,Inf,1,1), nrow=4, byrow=TRUE) 
)

Coalesce(d.frm)
Coalesce(as.matrix(d.frm))
Coalesce(d.frm[,2], d.frm[,3:4])
Coalesce(list(d.frm[,1], d.frm[,2]))

# returns the first finite element
Coalesce(d.frm, method="is.finite")

}
\keyword{ manip }
