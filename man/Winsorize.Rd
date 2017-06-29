\name{Winsorize}
\alias{Winsorize}
\title{Winsorize (Replace Extreme Values by Less Extreme Ones) }
\usage{
Winsorize(x, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
          na.rm = FALSE)
}
\arguments{
  \item{x}{a numeric vector to be winsorized.}

  \item{minval}{the low border, all values being lower than this will be replaced by this value.
  The default is set to the 5\%-quantile of x.}

  \item{maxval}{the high border, all values being larger than this will be replaced by this value.
  The default is set to the 95\%-quantile of x.}

  \item{probs}{numeric vector of probabilities with values in [0,1] as used in \code{\link{quantile}}.  }

  \item{na.rm}{should NAs be omitted to calculate the quantiles? \cr
  Note that NAs in x are preserved and left unchanged anyway. }
}

\value{
  A vector of the same length as the original data
  \code{x} containing the winsorized data.
}
\description{Winsorizing a vector means that
  a predefined quantum of the smallest and/or the largest values are replaced by less extreme values. Thereby the substitute values are the most extreme retained values.
}
\details{
  Consider standardizing (possibly robust) the data before winsorizing.
  See \code{\link{scale}}, \code{\link{RobScale}}
}

\seealso{
\code{\link{Winsorize}} \code{library(robustHD)} contains an option to winsorize multivariate data
%% ~~objects to See Also as \code{\link{help}}, ~~~
}


\examples{
## generate data
set.seed(1234)     # for reproducibility
x <- rnorm(10)     # standard normal
x[1] <- x[1] * 10  # introduce outlier

## Winsorize data
x
Winsorize(x)

# use Large and Small, if a fix number of values should be winsorized (here k=3):
Winsorize(x, minval=tail(Small(x, k=3), 1), maxval=head(Large(x, k=3), 1))
}
\author{
  Andri Signorell <andri@signorell.net> %, based on code by Gabor Grothendieck <ggrothendieck@gmail.com>
}

\keyword{univar}
\keyword{ robust }

