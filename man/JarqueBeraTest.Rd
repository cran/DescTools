\name{JarqueBeraTest}
\alias{JarqueBeraTest}
\title{(Robust) Jarque Bera Test}
\description{
This function performs the robust and classical Jarque-Bera tests of normality.
}
\usage{
JarqueBeraTest(x, robust = TRUE, method = c("chisq", "mc"), 
               N = 0, na.rm = FALSE)
}
\arguments{
  \item{x}{a numeric vector of data values.}
  \item{robust}{defines, whether the robust version should be used. 
  Default is TRUE.}
  \item{method}{a character string specifying how the critical 
                 values should be obtained, i.e. approximated by the
                 chisq-distribution (default) or empirically (\code{chisq} or \code{mc}).}
  \item{N}{number of Monte Carlo simulations for the empirical critical values}
  \item{na.rm}{defines if NAs should be omitted. Default is FALSE.}
}
\details{
The test is based on a joint statistic using skewness and kurtosis
coefficients. The Robust Jarque-Bera (RJB) is the robust version of
the Jarque-Bera (JB) test of normality. In particular, RJB utilizes
the robust standard deviation (namely the Average Absolute Deviation
from the Median (MAAD)) to estimate sample kurtosis and skewness
(default option). For more details see Gel and Gastwirth (2006).
\cr
Users can also choose to perform the classical Jarque-Bera test (see
Jarque, C. and Bera, A (1980)).
}
\value{
A list with class htest containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test was performed.}
  \item{data.name}{a character string giving the name of the data.}
}

\note{Modified from 'jarque.bera.test' (in 'tseries' package).}
\note{This is an exact copy of \code{rjb.test} from the package \code{lawstat}.}
\references{
Gastwirth, J. L.(1982) \emph{Statistical Properties of A Measure
of Tax Assessment Uniformity}, Journal of Statistical Planning
and Inference 6, 1-12.\cr

Gel, Y. R. and Gastwirth, J. L. (2008) \emph{A robust modification of 
the Jarque-Bera test of normality}, Economics Letters 99, 30-32.\cr

Jarque, C. and Bera, A. (1980) \emph{Efficient tests for
normality, homoscedasticity and serial independence of regression
residuals}, Economics Letters 6, 255-259.
}
\author{W. Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{
\code{\link{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
\code{\link{AndersonDarlingTest}}, \code{\link{CramerVonMisesTest}}, \code{\link{LillieTest}}, \code{\link{PearsonTest}}, \code{\link{ShapiroFranciaTest}} for performing further tests for normality.
\code{\link{qqnorm}}, \code{\link{qqline}} for producing a normal quantile-quantile plot.
}
\examples{
x <- rnorm(100)  # null
JarqueBeraTest(x)

x <- runif(100)  # alternative
JarqueBeraTest(x, robust=TRUE)
}
\keyword{htest}
