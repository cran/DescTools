\name{VarCI}
\alias{VarCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence Interval for the Variance
}
\description{
Calculates the confidence interval for the variance either the classical way or with the bootstrap approach.

}
\usage{
VarCI(x, method = c("classic", "norm", "basic", "stud", "perc", "bca"),
      conf.level = 0.95, sides = c("two.sided", "left", "right"),
      na.rm = FALSE, R = 999)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{method}{vector of character strings representing the type of intervals required. The value should be any subset of the values \code{"classic"}, \code{"norm"}, \code{"basic"},  \code{"stud"},  \code{"perc"},  \code{"bca"}.
  See \code{\link{boot.ci}}.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level of the interval.
%%     ~~Describe \code{conf.level} here~~
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.}

  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}
  \item{R}{number of bootstrap replicates. Usually this will be a single positive integer. For importance resampling, some resamples may use one set of weights and others use a different set of weights. In this case R would be a vector of integers where each component gives the number of resamples from each of the rows of weights.
  See \code{\link{boot}}.
%%     ~~Describe \code{x} here~~
}

}
\value{a numeric vector with 3 elements:
  \item{var}{variance}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}

\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{MeanCI}}, \code{\link{MedianCI}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
VarCI(d.pizza$price, na.rm=TRUE)
VarCI(d.pizza$price, conf.level=0.99, na.rm=TRUE)

x <- c(14.816, 14.863, 14.814, 14.998, 14.965, 14.824, 14.884, 14.838, 14.916,
       15.021, 14.874, 14.856, 14.860, 14.772, 14.980, 14.919)
VarCI(x, conf.level=0.9)

# and for the standard deviation
sqrt(VarCI(x, conf.level=0.9))

# some bootstrap intervals
VarCI(x, method="norm")
VarCI(x, method="perc")
VarCI(x, method="bca")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
