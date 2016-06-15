\name{HighLow}
\alias{HighLow}

\title{Return the Lowest and the Highest Values and Their Frequencies
}
\description{A printing routine for the highest and the lowest values of x.
It enumerates the according values and their frequencies (in brackets).
}
\usage{
HighLow(x, nlow = 5, nhigh = nlow, na.last = NA)
}
\arguments{
  \item{x}{a numeric vector or an ordered factor.
}
  \item{nlow}{a single integer. The number of the smallest elements of a vector to be printed. Defaults to 5.
}
  \item{nhigh}{a single integer. The number of the greatest elements of a vector to be printed. Defaults to the number of \code{nlow}.
}
\item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed.}
}
\details{This is used for describing univariate variables and is interesting for checking the ends of the vector, where in real data often wrong values accumulate.}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{min}}, \code{\link{max}}, \code{\link{table}}
}
\examples{
cat(HighLow(d.pizza$temperature, na.last=NA))
}
\keyword{ arith }
