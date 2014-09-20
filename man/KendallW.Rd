\name{KendallW}
\alias{KendallW}
\title{Kendall's Coefficient of Concordance W}
\description{
Computes Kendall's coefficient of concordance as an index of interrater reliability of ordinal data. The coefficient could be corrected for ties within raters.
}
\usage{
KendallW(ratings, correct = FALSE, test = FALSE)
}
\arguments{
  \item{ratings}{n*m matrix or dataframe, n subjects m raters.}
  \item{correct}{a logical indicating whether the coefficient should be corrected for ties within raters.}
  \item{test}{a logical indicating whether the test statistic and p-value should be reported.}
}
\details{
Missing data are omitted in a listwise way.\cr
Kendall's W should be corrected for ties if raters did not use a true ranking order for the subjects.\cr
A test for the significance of Kendall's W is only valid for large samples. 
}
\value{
Either a single value if test is set to FALSE or else \cr

a list with class \dQuote{htest} containing the following components: 
  \item{statistic}{the value of the chi-square statistic.}
  \item{p.value }{the p-value for the test.}
  \item{method}{the character string \dQuote{Kendall's coefficient of concordance W}.}
  \item{data.name}{a character string giving the name(s) of the data.} 
  \item{estimate}{the coefficient of concordance.}
  \item{parameter}{the degrees of freedom df, the number of subjects examined and the number of raters.}
}
\note{
This function was previously published as \code{kendall()} in the  \pkg{irr} package and has been
integrated here without logical changes, but with some adaptations in the result structure.
}

\references{
Kendall, M.G. (1948) Rank correlation methods. London: Griffin.
}
\author{Matthias Gamer}
\seealso{
  \code{\link[stats]{cor}}  }

\examples{
anxiety <- data.frame(rater1=c(3,3,3,4,5,5,2,3,5,2,2,6,1,5,2,2,1,2,4,3),
                      rater2=c(3,6,4,6,2,4,2,4,3,3,2,3,3,3,2,2,1,3,3,4),
                      rater3=c(2,1,4,4,3,2,1,6,1,1,1,2,3,3,1,1,3,3,2,2))

KendallW(anxiety, TRUE)

# with test results
KendallW(anxiety, TRUE, test=TRUE)
}
\keyword{multivar}
