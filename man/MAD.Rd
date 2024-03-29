% File src/library/stats/man/MAD.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{MAD}
\title{Median Absolute Deviation}
\usage{
MAD(x, weights = NULL, center = Median, constant = 1.4826, 
    na.rm = FALSE, low = FALSE, high = FALSE)
}
\alias{MAD}
\arguments{
  \item{x}{a numeric vector.}
   \item{weights}{a numerical vector of weights the same length as \code{x} giving the weights to use for elements of \code{x}.
 }

  \item{center}{the centre given either as numeric value or as a function to be applied to \code{x} (defaults to the \code{DescTools::Median(x)}). Note in cases when weights are defined to provide a function that also support weights. If this is not possible fall back to a numeric value.}
  \item{constant}{scale factor (default is \code{1.4826})}
  \item{na.rm}{if \code{TRUE} then \code{NA} values are stripped
    from \code{x} before computation takes place.}
  \item{low}{if \code{TRUE}, compute the \sQuote{lo-median}, i.e., for even
    sample size, do not average the two middle values, but take the
    smaller one.}
  \item{high}{if \code{TRUE}, compute the \sQuote{hi-median}, i.e., take the
    larger of the two middle values for even sample size.}
}
\description{
  Compute the median absolute deviation, i.e., the (lo-/hi-) median of
  the absolute deviations from the median, and (by default) adjust by a
  factor for asymptotically normal consistency. This function wraps the specific base R function \code{\link{mad}} and extends it for the use of weights. 
}
\details{
  The actual value calculated is \code{constant * cMedian(abs(x - center))}
  with the default value of \code{center} being \code{median(x)}, and
  \code{cMedian} being the usual, the \sQuote{low} or \sQuote{high} median, see
  the arguments description for \code{low} and \code{high} above.

  The default \code{constant = 1.4826} (approximately
  \eqn{1/\Phi^{-1}(\frac 3 4)}{1/ \Phi^(-1)(3/4)} = \code{1/qnorm(3/4)})
  ensures consistency, i.e.,
  \deqn{E[mad(X_1,\dots,X_n)] = \sigma}
  for \eqn{X_i} distributed as \eqn{N(\mu, \sigma^2)}
  and large \eqn{n}.

  If \code{na.rm} is \code{TRUE} then \code{NA}
  values are stripped from \code{x} before computation takes place.
  If this is not done then an \code{NA} value in
  \code{x} will cause \code{MAD} to return \code{NA}.
}
\seealso{
  \code{\link{IQR}} which is simpler but less robust, \code{\link{IQRw}} for weights, 
  \code{\link{mad}}, \code{\link{median}}, \code{\link{var}}, \code{\link{MADCI}} (confidence intervals).
}
\examples{
MAD(c(1:9))
print(MAD(c(1:9),     constant = 1)) ==
      MAD(c(1:8, 100), constant = 1)       # = 2 ; TRUE
x <- c(1,2,3,5,7,8)
sort(abs(x - median(x)))
c(MAD(x, constant = 1),
  MAD(x, constant = 1, low = TRUE),
  MAD(x, constant = 1, high = TRUE))

# use weights
x <- sample(20, 30, replace = TRUE)
z <- as.numeric(names(w <- table(x)))

(m1 <- MAD(z, weights=w))
(m2 <- MAD(x))
stopifnot(identical(m1, m2))

}
\keyword{univar}
\keyword{robust}
