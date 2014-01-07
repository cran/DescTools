\name{ZTest}
\alias{ZTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Z Test for Known Population Standard Deviation }
\description{
  Compute the test of hypothesis and compute confidence interval on the
  mean of a population when the standard deviation of the population is known.
}
\usage{
ZTest(x, mu = 0, sd_pop, alternative = c("two.sided", "less", "greater"), 
      conf.level = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Vector of data values. }
  \item{mu}{ Hypothesized mean of the population. }
  \item{sd_pop}{ Known standard deviation of the population. }
  \item{alternative}{ Direction of the alternative hypothesis. }
  \item{conf.level}{ Confidence level for the interval computation. }
}
\details{
  Most introductory statistical texts introduce inference by using the z-test 
  and z-based confidence intervals based on knowing the population
  standard deviation.  Most statistical packages do not include
  functions to do z-tests since the t-test is usually more appropriate
  for real world situations.  This function is meant to be used during
  that short period of learning when the student is learning about
  inference using z-procedures, but has not learned the t-based
  procedures yet.  Once the student has learned about the 
  t-distribution the \code{t.test} function should be used instead of this
  one (but the syntax is very similar, so this function should be an
  appropriate introductory step to learning \code{t.test}).
}
\value{
  An object of class \code{htest} containing the results
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow \email{greg.snow@imail.org} }
\note{ This function should be used for learning only, real data should
  generally use \code{t.test}. }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{t.test}}, \code{\link{print.htest}} }
\examples{

x <- rnorm(25, 100, 5)
ZTest(x, 99, 5)

}
\keyword{ htest }

