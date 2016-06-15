\name{Range}
\alias{Range}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Range
%%  ~~function to do ... ~~
}
\description{
Generic function for the (trimmed) range.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Range(x, trim = 0, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector.}
  \item{trim}{the fraction (0 to 0.5) of observations to be
    trimmed from each end of \code{x} before the mean is computed.
    Values of trim outside that range are taken as the nearest endpoint.
  }
  \item{na.rm}{a logical value indicating whether \code{NA}
    values should be stripped before the computation proceeds.}
}
\details{The R base function range returns the minimum and maximum value of a numeric object. Here we return the span of a (possibly trimmed) numeric vector, say the difference of maximum and minimum value.
}
\value{
  If \code{trim} is zero (the default), the arithmetic mean of the
  values in \code{x} is computed, as a numeric or complex vector of
  length one.  If \code{x} is not logical (coerced to numeric), numeric
  (including integer) or complex, \code{NA_real_} is returned, with a warning.

  If \code{trim} is non-zero, a symmetrically trimmed mean is computed
  with a fraction of \code{trim} observations deleted from each end
  before the mean is computed.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{range}}, \code{\link{min}}, \code{\link{max}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  x <- c(0:10, 50)
  xm <- Range(x)
  c(xm, Range(x, trim = 0.10))
}
\keyword{ univar}
