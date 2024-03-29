
% Generated by roxygen2 (4.0.1.99): do not edit by hand
\name{Quantile}
\alias{Quantile}
\title{(Weighted) Sample Quantiles}
\usage{
Quantile(x, weights = NULL, probs = seq(0, 1, 0.25), 
         na.rm = FALSE, names = TRUE, type = 7, digits = 7)
}
\arguments{
  \item{x}{a numeric vector.}

  \item{weights}{an optional numeric vector giving the sample weights.}

  \item{probs}{numeric vector of probabilities with values in \eqn{[0,1]}.}

  \item{na.rm}{a logical indicating whether missing values in \code{x} should
    be omitted.}

  \item{names}{logical; if true, the result has a \code{\link{names}} attribute. 
      Set to \code{FALSE} for speedup with many \code{probs}.}

  \item{type}{an integer between 1 and 9 selecting one of the nine quantile algorithms detailed below to be used. Currently only \code{types} \code{5} and \code{7} (default) are implemented.}
  
  \item{digits}{used only when \code{names} is true: the precision to use when formatting the percentages. In \code{R} versions up to 4.0.x, this had been set to \code{max(2, getOption("digits"))}, internally.}

}

%  \item{sorted}{a logical indicating whether the observations in \code{x} are
%    already sorted.}

\value{
  A named numeric vector containing the weighted quantiles of values in
  \code{x} at probabilities \code{probs} is returned.  
}
\description{
  Compute weighted quantiles (Eurostat definition).
}
\details{
  The implementation strictly follows the Eurostat definition.
}
\examples{
Quantile(d.pizza$temperature, rep(c(1:3), length.out=nrow(d.pizza)))
}
\author{
  Andreas Alfons, Matthias Templ, some tweaks Andri Signorell <andri@signorell.net>
}
\references{
  Working group on Statistics on Income and Living Conditions
  (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay
  gap.  \emph{EU-SILC 131-rev/04}, Eurostat.
}
\seealso{
  \code{\link{Median}}, \code{\link[stats]{quantile}}, \code{\link{QuantileCI}}
}
\keyword{univar}


