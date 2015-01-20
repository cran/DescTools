\name{LogSt}
\alias{LogSt}
\alias{LogStInv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Started Logarithmic Transformation and Its Inverse
%%  ~~function to do ... ~~
}
\description{Transforms the data by a log10 transformation, modifying small and zero observations such that the
transformation yields finite values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LogSt(x, calib = x, threshold = NULL, mult = 1)

LogStInv(x, threshold = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector or matrix of data, which is to be transformed
%%     ~~Describe \code{x} here~~
}
  \item{calib}{a vector or matrix of data used to calibrate the transformation(s), i.e., to determine
the constant c needed
%%     ~~Describe \code{calib} here~~
}
  \item{threshold}{constant c that determines the transformation. The inverse function will
  look for an attribute named "threshold" if the argument is set to NULL.
%%     ~~Describe \code{threshold} here~~
}
  \item{mult}{a tuning constant affecting the transformation of small values, see Details
%%     ~~Describe \code{mult} here~~
}
}
\details{Small values are determined by the threshold c. If not given by the argument threshold, then it is determined by the quartiles q1 and q3 of the non-zero data as those smaller than c = q1=(q3=q1)mult.
The rationale is that for lognormal data, this constant identifies 2 percent of the data as small.
Beyond this limit, the transformation continues linear with the derivative of the log curve at this
point. See code for the formula.\cr
Another possible value for the threshold c was: median(x) / (median(x)/quantile(x, 0.25))^2.9)\cr\cr
The function chooses log10 rather than natural logs because they can be backtransformed relatively
easily in the mind.

%%  ~~ If necessary, more details than the description above ~~
}
\value{the transformed data. The value c needed for the transformation is returned as \code{attr(.,"threshold")}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\note{The names of the function alludes to Tukey's idea of "started logs".
}
\author{Werner A. Stahel, ETH Zurich, slight modifications Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{LogLin}}
%% ~~objects to See Also as , ~~~
}
\examples{
dd <- c(seq(0,1,0.1), 5 * 10^rnorm(100, 0, 0.2))
dd <- sort(dd)
r.dl <- LogSt(dd)
plot(dd, r.dl, type="l")
abline(v=attr(r.dl, "threshold"), lty=2)

x <- rchisq(df=3, n=100)
# should give 0 (or at least something small):
LogStInv(LogSt(x)) - x
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
