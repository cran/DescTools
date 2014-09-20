\name{HighLow}
\alias{HighLow}

\title{Return the Lowest and the Highest Values and Their Frequencies
}
\description{A printing routine for the highest and the lowest values of x. 
It enumerates the according values and their frequencies (in brackets).
}
\usage{
HighLow(x, nlow = 5, nhigh = nlow, na.rm = FALSE)
}
\arguments{
  \item{x}{a numeric vector or an ordered factor.
}
  \item{nlow}{a single integer. The number of the smallest elements of a vector to be printed. Defaults to 5.
}
  \item{nhigh}{a single integer. The number of the greatest elements of a vector to be printed. Defaults to the number of \code{nlow}.
}
  \item{na.rm}{logical, indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE. 
%%     ~~Describe \code{na.rm} here~~
}

}
\details{This is used for describing univariate variables and is interesting for checking the ends of the vector, where in real data often wrong values accumulate.}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{min}}, \code{\link{max}}, \code{\link{table}}
}
\examples{
cat(HighLow(d.pizza$temperature, na.rm=TRUE))
}
\keyword{ arith }
