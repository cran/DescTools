\name{HodgesLehmann}
\alias{HodgesLehmann}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hodges-Lehmann Estimator of Location}
\description{
Function to compute the Hodges-Lehmann estimator of location in the one and two sample case following a clever fast algorithm by John Monahan (1984). 
}
\usage{
HodgesLehmann(x, y = NULL, conf.level = NA, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numeric vector.}
  \item{y}{an optional numeric vector of data values: as with x non-finite values will be omitted.}
  
   \item{conf.level}{confidence level of the interval.
%%     ~~Describe \code{conf.level} here~~
}

  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}
}
\details{
  The Hodges-Lehmann estimator is the median of the combined data points and Walsh averages.
  It is the same as the Pseudo Median returned as a by-product of the function \code{\link{wilcox.test}} (which however does not calculate correctly as soon as ties are present).\cr
  Note that in the two-sample case the estimator for the difference in location parameters does not estimate the difference in medians (a common misconception) but rather the median of the difference between a sample from x and a sample from y.

(The calculation of the confidence intervals is not yet implemented.)
}
\value{
  the Hodges-Lehmann estimator of location as a single numeric value if no confidence intervals are requested,\cr 
and otherwise a numeric vector with 3 elements for the estimate, the lower and the upper confidence interval

}
\references{ 
Hodges, J.L., and Lehmann, E.L. (1963), Estimates of location based on rank tests. \emph{The Annals of Mathematical Statistics}, \bold{34}, 598--611. 

Monahan, J. (1984), Algorithm 616: Fast Computation of the Hodges-Lehmann Location Estimator, \emph{ACM Transactions on Mathematical Software}, Vol. 10, No. 3, pp. 265-270
}


\author{ Cyril Flurin Moser (Cyril did the lion's share and coded Monahan's algorithm in C++), 
 Andri Signorell <andri@signorell.net> }

\seealso{\code{\link{wilcox.test}}, \code{\link{median}}, \code{\link{MedianCI}}}
\examples{
set.seed(1)
x <- rt(100, df = 3)
y <- rt(100, df = 5)

HodgesLehmann(x)
HodgesLehmann(x, y)

# same as
wilcox.test(x, conf.int = TRUE)$estimate
}
\keyword{nonparametric}
\keyword{robust}
\keyword{univar}
