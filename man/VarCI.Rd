\name{VarCI}
\alias{VarCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence Intervals for the Variance
}
\description{
Calculates confidence intervals for the variance. Available approachs are the classical one using the ChiSquare distribution, a more robust version proposed by Bonett and the bootstrap options available in the package \code{boot}.

}
\usage{
VarCI(x, method = c("classic", "bonett", "norm", "basic", "stud", "perc", "bca"),
      conf.level = 0.95, sides = c("two.sided", "left", "right"),
      na.rm = FALSE, R = 999)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{method}{vector of character strings representing the type of intervals required. The value should be any subset of the values \code{"classic"}, \code{"bonett"}, \code{"norm"}, \code{"basic"},  \code{"stud"},  \code{"perc"},  \code{"bca"}.
  See \code{\link[boot]{boot.ci}}.
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
  See \code{\link[boot]{boot}}.
%%     ~~Describe \code{x} here~~
}

}
\value{a numeric vector with 3 elements:
  \item{var}{variance}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}

\details{The confidence interval for the variance is very sensitive to non-normality in the data. Bonett (2006) has proposed an interval that is nearly exact when the data is normally distributed and provides good performance for moderately non-normal data.
See the references for the details.}


\references{
Bonett (2006) Approximate Confidence Interval for Standard Deviation of Nonnormal Distributions, \emph{Computational Statistics and Data Analysis}, Vol. 50, pp. 775 - 782.\cr
https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/sdconfli.htm (might be outdated)
}

\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{MeanCI}}, \code{\link{MedianCI}}, \code{\link{VarTest}}, \code{\link{Var}}
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


# from Bonett's paper
# expected results:
# ------------------------------------
#  conf.lvl       sd      lci      uci
# ------------------------------------
#      90.0   0.5168   0.3592   0.9359
#      95.0   0.5168   0.3263   1.0841
#      99.0   0.5168   0.2607   1.5109

p <- c(15.83, 16.01, 16.24, 16.42, 15.33, 15.44, 16.88, 16.31)
sqrt(VarCI(p, method="bonett", conf.level=0.9))
sqrt(VarCI(p, method="bonett"))
sqrt(VarCI(p, method="bonett", conf.level=0.99))

# some bootstrap intervals
VarCI(x, method="norm")
VarCI(x, method="perc")
VarCI(x, method="bca")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
