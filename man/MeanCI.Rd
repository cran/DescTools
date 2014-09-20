\name{MeanCI}
\alias{MeanCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confidence Interval for the Mean
%%  ~~function to do ... ~~
}
\description{
Calculates the confidence interval for the mean either the classical way or with the bootstrap approach. 
}
\usage{
MeanCI(x, trim = 0, method = c("classic", "norm", "basic", "stud", "perc", "bca"), 
       conf.level = 0.95, na.rm = FALSE, R = 999) 
 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{trim}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{method}{A vector of character strings representing the type of intervals required. The value should be any subset of the values \code{"classic"}, \code{"norm"}, \code{"basic"},  \code{"stud"},  \code{"perc"},  \code{"bca"}.
  See \code{\link{boot.ci}}.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level of the interval.
%%     ~~Describe \code{conf.level} here~~
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}
  \item{R}{The number of bootstrap replicates. Usually this will be a single positive integer. For importance resampling, some resamples may use one set of weights and others use a different set of weights. In this case R would be a vector of integers where each component gives the number of resamples from each of the rows of weights.
  See \code{\link{boot}}.
%%     ~~Describe \code{x} here~~
}
}
\details{Interfaces for data.frames are widely deprecated nowadays and so we abstained to implement one. 
Use \code{\link{do.call}}, \code{\link{rbind}} and \code{\link{lapply}} for getting a 
matrix with estimates and confidence intervals for more than 1 column. (See examples!)
%%  ~~ If necessary, more details than the description above ~~
}
\value{a numeric vector with 3 elements:
  \item{mean}{mean}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}
\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{MeanDiffCI}}, \code{\link{MedianCI}}, \code{\link{VarCI}}
}

\examples{
x <- d.pizza$price[1:20]

MeanCI(x, na.rm=TRUE)
MeanCI(x, conf.level=0.99, na.rm=TRUE)

# the different types of bootstrap confints
MeanCI(x, method="norm", na.rm=TRUE)
MeanCI(x, method="norm", trim=0.1, na.rm=TRUE)
MeanCI(x, method="basic", trim=0.1, na.rm=TRUE)
# MeanCI(x, method="stud", trim=0.1, na.rm=TRUE), this needs some more information
MeanCI(x, method="perc", trim=0.1, na.rm=TRUE)
MeanCI(x, method="bca", trim=0.1, na.rm=TRUE)


# Getting the MeanCI for more than 1 column
round( do.call("rbind", lapply(d.pizza[,1:4],  MeanCI, na.rm=TRUE)), 3)
}
\keyword{univar }
