\name{JarqueBeraTest}
\alias{JarqueBeraTest}
\title{Jarque--Bera Test}
\description{
  Tests the null of normality for \code{x} using the Jarque-Bera
  test statistic.
}
\usage{
JarqueBeraTest(x)
}
\arguments{
  \item{x}{a numeric vector or time series.}
}
\details{
  This test is a joint statistic using skewness and kurtosis
  coefficients.
  
  Missing values are not allowed.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom.}
  \item{p.value}{the p-value of the test.}
  \item{method}{a character string indicating what type of test was
    performed.} 
  \item{data.name}{a character string giving the name of the data.}
}
\references{
Cromwell, J B,  Labys, W C and Terraza, M (1994):
  \emph{Univariate Tests for Time Series Models},
  Sage, Thousand Oaks, CA, pages 20--22.
}
\note{ This function was previously published as \code{jarque.bera.test}() in the  \pkg{tseries} package and has been integrated here without logical changes. 
}
\author{Adrian Trapletti}

\examples{
x <- rnorm(100)  # null
JarqueBeraTest(x)

x <- runif(100)  # alternative
JarqueBeraTest(x)
}

\keyword{ts}
\keyword{htest}
