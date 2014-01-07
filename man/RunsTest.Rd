\name{RunsTest}
\alias{RunsTest}
\title{Runs Test for Randomness}
\description{
This function performs the runs test for randomness.  
}
\usage{
RunsTest(x, alternative = c("two.sided", "less", "greater"), na.rm = FALSE)
}
\arguments{
  \item{x}{a dichotomous vector of data values. }
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"less"} or \code{"greater"}.} 
  \item{na.rm}{defines if NAs are omitted. Default is FALSE. }
}


\details{
The runs test for randomness, also known as the Wald-Wolfowitz test, is used to test the hypothesis 
that a series of numbers is random. \cr

For a categorical variable, the number of runs correspond to the number of times the category changes, that is, 
where xi belongs to one category and xi+1 belongs to the other. The number of runs, is the number of sign changes plus one.\cr

For a numeric variable x, a run is a set of sequential values that are either all above or below a 
specified cutpoint, typically the median. 
}

\value{
A list with the following components.
  \item{statistic}{the value of the standardized runs statistic.}
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
  Andri Signorell <andri@signorell.net>
}

\examples{

# x has to be dichotomous for the test being 
x <- c("S","S", "T", "S", "T","T","T", "S", "T")
RunsTest( x < median(x) )


x <- c(13, 3, 14, 14, 1, 14, 3, 8, 14, 17, 9, 14, 13, 2, 16, 1, 3, 12, 13, 14)
RunsTest( x < median(x) )

plot( (x < median(x)) - 0.5, type="s", ylim=c(-1,1) )
abline(h=0)

set.seed(123)
x <- sample(0:1, size=100, replace=TRUE)
RunsTest(x)
# As you would expect of values from a random number generator, the test fails to reject 
# the null hypothesis that the data are random. 

}
\keyword{htest}

