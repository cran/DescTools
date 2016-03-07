\name{RunsTest}
\alias{RunsTest}
\alias{RunsTest.formula}
\alias{RunsTest.default}
\title{Runs Test for Randomness}
\description{
Performs a one sample runs test or a two sample Wald-Wolfowitz-Test on vectors of data.
}
\usage{
RunsTest(x, ...)

\method{RunsTest}{default}(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         exact = NULL, na.rm = FALSE, ...)

\method{RunsTest}{formula}(formula, data, subset, na.action, \dots)

}
\arguments{
  \item{x}{a dichotomous vector of data values or a (non-empty) numeric vector of data values.}
  \item{y}{an optional (non-empty) numeric vector of data values.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}

  \item{alternative}{a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"less"} or \code{"greater"}.}
  \item{exact}{a logical indicating whether an exact p-value should be computed. By default exact values will be calculated for small vectors with a total length <= 30.}
  \item{na.rm}{defines if \code{NA}s should be omitted. Default is \code{FALSE}. }
  \item{\dots}{further arguments to be passed to or from methods.}

}


\details{
The runs test for randomness is used to test the hypothesis
that a series of numbers is random. The 2-sample test is known as the Wald-Wolfowitz test. \cr

For a categorical variable, the number of runs correspond to the number of times the category changes, that is,
where \eqn{x_{i}} belongs to one category and  \eqn{x_{i+1}}  belongs to the other. The number of runs, is the number of sign changes plus one.\cr

For a numeric variable x containing more than two values, a run is a set of sequential values that are either all above or below a
specified cutpoint, typically the median.
}

\value{
A list with the following components.
  \item{statistic}{z, the value of the standardized runs statistic, if not exact p-values are computed.}
  \item{parameter}{the number of runs, the total number of zeros (m) and ones (n)}
  \item{p.value}{the p-value for the test.}
  \item{data.name}{a character string giving the names of the data.}
  \item{alternative}{a character string describing the alternative hypothesis.}
}
\references{
Wackerly, D., Mendenhall, W. Scheaffer, R. L. (1986): \emph{Mathematical Statistics with Applications}, 3rd Ed., Duxbury Press, CA.

Wald, A. and Wolfowitz, J. (1940): On a test whether two samples are from the same population,  \emph{Ann. Math Statist}. 11, 147-162.

}

\seealso{
  Run Length Encoding \code{\link{rle}}
}

\author{
  Andri Signorell <andri@signorell.net>, exact p-values by Detlew Labes <detlewlabes@gmx.de>
}

\examples{
# x will be coerced to a dichotomous variable
x <- c("S","S", "T", "S", "T","T","T", "S", "T")
RunsTest(x)


x <- c(13, 3, 14, 14, 1, 14, 3, 8, 14, 17, 9, 14, 13, 2, 16, 1, 3, 12, 13, 14)
RunsTest(x)
# this will be treated as
RunsTest(x < median(x))

plot( (x < median(x)) - 0.5, type="s", ylim=c(-1,1) )
abline(h=0)

set.seed(123)
x <- sample(0:1, size=100, replace=TRUE)
RunsTest(x)
# As you would expect of values from a random number generator, the test fails to reject
# the null hypothesis that the data are random.


# SPSS example
x <- c(31,23,36,43,51,44,12,26,43,75,2,3,15,18,78,24,13,27,86,61,13,7,6,8)
RunsTest(x)
RunsTest(x, exact=TRUE)

# SPSS example small dataset
x <- c(1, 1, 1, 1, 0, 0, 0, 0, 1, 1)
RunsTest(x)
RunsTest(x, exact=FALSE)

# if y is not NULL, the Wald-Wolfowitz-Test will be performed
A <- c(35,44,39,50,48,29,60,75,49,66)
B <- c(17,23,13,24,33,21,18,16,32)

RunsTest(A, B, exact=TRUE)
RunsTest(A, B, exact=FALSE)
}
\keyword{htest}

