\name{MeanDiffCI}
\alias{MeanDiffCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence Intervals for Difference of Means
%%  ~~function to do ... ~~
}
\description{Calculates the confidence interval for the difference of two means either the classical way or with the bootstrap approach.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MeanDiffCI(x, y, type = c("classic", "norm", "basic", "stud", "perc", "bca"), 
           conf.level = 0.95, na.rm = FALSE, R = 999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{y}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{y} here~~
}
  \item{type}{a vector of character strings representing the type of intervals required. The value should be any subset of the values 
  \code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}.
  See \code{\link{boot.ci}}.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level of the interval.
%%     ~~Describe \code{conf.level} here~~
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}
  \item{R}{the number of bootstrap replicates. Usually this will be a single positive integer. For importance resampling, some resamples may use one set of weights and others use a different set of weights. In this case R would be a vector of integers where each component gives the number of resamples from each of the rows of weights.
  See \code{\link{boot}}.
%%     ~~Describe \code{x} here~~
}
}
\details{This function collects code from two sources. The classical confidence interval is calculated by means of \code{\link{t.test}}.
The bootstrap intervals are strongly based on the example in \code{\link{boot}}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a numeric vector with 3 elements:
  \item{meandiff}{the difference: mean(x) - mean(y)}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}
\author{Andri Signorell <andri@signorell.net>}

\seealso{ 
\code{\link{MeanCI}}, \code{\link{VarCI}}, \code{\link{MedianCI}}, \code{\link{boot.ci}}
}

\examples{
x <- d.pizza$price[d.pizza$driver=="Carter"]
y <- d.pizza$price[d.pizza$driver=="Miller"]

MeanDiffCI(x, y, na.rm=TRUE)
MeanDiffCI(x, y, conf.level=0.99, na.rm=TRUE)

# the different types of bootstrap confints
MeanDiffCI(x, y, type="norm", na.rm=TRUE)
MeanDiffCI(x, y, type="basic", na.rm=TRUE)
# MeanDiffCI(x, y, type="stud", na.rm=TRUE)
MeanDiffCI(x, y, type="perc", na.rm=TRUE)
MeanDiffCI(x, y, type="bca", na.rm=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar }% __ONLY ONE__ keyword per line
