\name{BoxCox}
\alias{BoxCox}
\alias{BoxCoxInv}
\title{Box Cox Transformation}
\usage{BoxCox(x, lambda)
BoxCoxInv(x, lambda)
}
\arguments{
\item{x}{a numeric vector }
\item{lambda}{transformation parameter}
}
\description{BoxCox() returns a transformation of the input variable using a Box-Cox transformation.\cr
BoxCoxInv() reverses the transformation.
}
\details{The Box-Cox transformation is given by
\deqn{f_\lambda(x) =\frac{x^\lambda - 1}{\lambda}}{f(x;lambda)=(x^lambda - 1)/lambda}
if \eqn{\lambda\ne0}{lambda is not equal to 0}. For \eqn{\lambda=0}{lambda=0},
\deqn{f_0(x)=\log(x)}{f(x;0)=log(x)}.
}
\value{a numeric vector of the same length as x.
}
\references{Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. \emph{JRSS B} \bold{26} 211--246.
}
\seealso{Use \code{\link{BoxCoxLambda}} or \code{\link[MASS]{boxcox}} in library(MASS) to find optimal lambda values. }
\note{These two functions are borrowed from library(forecast).}
\author{Rob J Hyndman <rob.hyndman@monash.edu>}

\examples{
# example by Greg Snow
x <- rlnorm(500, 3, 2)

par(mfrow=c(2,2))
qqnorm(x, main="Lognormal")
qqnorm(BoxCox(x, 1/2), main="BoxCox(lambda=0.5)")
qqnorm(BoxCox(x, 0), main="BoxCox(lambda=0)")

hist(BoxCox(x, 0))

bx <- BoxCox( x, lambda = BoxCoxLambda(x) )
}
\keyword{univar}



