\name{AndersonDarlingTest}
\alias{AndersonDarlingTest}
\title{Anderson-Darling test for normality}
\description{
  Performs the Anderson-Darling test for  the composite hypothesis of normality, 
  see e.g. Thode (2002, Sec. 5.1.4).
}
  
\usage{
AndersonDarlingTest(x)
}
\arguments{
  \item{x}{a numeric vector of data values, the number of 
  which must be greater than 7. Missing values are allowed.}
}
\details{The Anderson-Darling test is an EDF omnibus test for the composite hypothesis of normality. 
The test statistic is 
\deqn{
A = -n -\frac{1}{n} \sum_{i=1}^{n} [2i-1] 
[\ln(p_{(i)}) + \ln(1 - p_{(n-i+1)})],
}
where \eqn{p_{(i)} = \Phi([x_{(i)} - \overline{x}]/s)}. Here, 
\eqn{\Phi} is the cumulative distribution function 
of the standard normal distribution, and \eqn{\overline{x}} and \eqn{s} 
are mean and standard deviation of the data values. 
The p-value is computed from the modified statistic 
\eqn{Z=A (1.0 + 0.75/n +2.25/n^{2})}\ according to Table 4.9 in 
Stephens (1986). 

}
\value{
  A list with class \dQuote{htest} containing the following components: 
  \item{statistic}{the value of the Anderson-Darling statistic.}
  \item{p.value}{the p-value for the test.}
  \item{method}{the character string \dQuote{Anderson-Darling normality test}.}
  \item{data.name}{a character string giving the name(s) of the data.} 
}
\references{Stephens, M.A. (1986) \emph{Tests based on EDF statistics. In:
D'Agostino, R.B. and Stephens, M.A., eds.: Goodness-of-Fit Techniques}.
Marcel Dekker, New York.

Thode Jr., H.C. (2002) \emph{Testing for  Normality}. Marcel Dekker, New York.
}

\note{The Anderson-Darling test is the recommended EDF test by Stephens (1986). Compared to the 
Cramer-von Mises test (as second choice) it gives more weight to the tails of the distribution.}

\note{ This function was previously published as \code{ad.test()} in the  \pkg{nortest} package and has been integrated here without logical changes. 
}

\author{Juergen Gross <gross@statistik.uni-dortmund.de>}


\seealso{\code{\link{shapiro.test}} for performing the Shapiro-Wilk test for normality. 
\code{\link{CramerVonMisesTest}}, \code{\link{LillieTest}}, 
\code{\link{PearsonTest}}, \code{\link{ShapiroFranciaTest}} for performing further tests for normality.
\code{\link{qqnorm}} for producing a normal quantile-quantile plot.}

\examples{
AndersonDarlingTest(rnorm(100, mean = 5, sd = 3))
AndersonDarlingTest(runif(100, min = 2, max = 4))

}
\keyword{htest}
