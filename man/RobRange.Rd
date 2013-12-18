\name{RobRange}
\alias{RobRange}
\title{Robust Range
}
\description{Determines a robust range of the data on the basis of the trimmed mean and variance.
}
\usage{
RobRange(x, trim = 0.2, fac = 3, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of data. 
%%     ~~Describe \code{data} here~~
}
  \item{trim}{the fraction (0 to 0.5) of observations to be trimmed from each end of x before the mean is computed. Values of trim outside that range are taken as the nearest endpoint. Default is 0.2.
%%     ~~Describe \code{trim} here~~
}
  \item{fac}{factor used for expanding the range, see Details. Default is 3.
}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
}

\details{
The function determines the trimmed mean m and then the "upper trimmed mean" s of absolute deviations
from m, multiplied by \code{fac}. The robust minimum is then defined as m-fac*s or min(x),
whichever is larger, and similarly for the maximum.
}
\value{The robust range.
}
\author{Werner Stahel, ETH Zurich
}


\seealso{\code{\link{RobScale}}
}
\examples{
x <- c(rnorm(20), rnorm(3, 5, 20))
RobRange(x)

# compared to:
range(x)

}
\keyword{ robust }
