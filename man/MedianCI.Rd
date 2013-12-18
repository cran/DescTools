\name{MedianCI}
\alias{MedianCI}
\title{
Confidence Interval for the Median
}
\description{
Calculates the confidence interval for the median. 
}
\usage{
MedianCI(x, conf.level = 0.95, na.rm = FALSE, 
         type = c("pseudo", "exact", "boot"), R = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values. 
}
  \item{conf.level}{confidence level of the interval
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE. 
}
  \item{type}{defining the type of interval that should be calculated. Default is \code{"pseudo"}. See Details.}
  
\item{R}{The number of bootstrap replicates. Usually this will be a single positive integer. See
\code{\link{boot.ci}} for details.
}
  
}
\details{
The the confidence interval for the "pseudo median"" is extracted from \code{\link{wilcox.test}} (\code{conf.int = TRUE}). (Would you have expected it there?)\cr
The exact type is the way SAS is said to calculate the confidence interval. This is implemented in \code{\link{SignTest}} and is extracted from there.
The boot confidence interval type is calculated by means of \code{\link{boot.ci}} with type \code{"basic"}.\cr
Use \code{\link{sapply}}, resp.\code{\link{apply}}, to get the confidence intervals from a data.frame or from a matrix.
}
\value{a numeric vector with 3 elements:
  \item{median}{median}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}

\author{
Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{wilcox.test}}, \code{\link{MeanCI}}, \code{\link{median}}
}
\examples{
data(d.pizza)

MedianCI(d.pizza$price, na.rm=TRUE)
MedianCI(d.pizza$price, conf.level=0.99, na.rm=TRUE)

t(round(sapply(d.pizza[,c("delivery_min","temperature","price")], MedianCI, na.rm=TRUE), 3))


MedianCI(d.pizza$price, na.rm=TRUE, type="exact")
MedianCI(d.pizza$price, na.rm=TRUE, type="boot")


}
\keyword{ univar }
