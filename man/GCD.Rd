\name{GCD, LCM}
\alias{GCD}
\alias{LCM}

\title{Greatest Common Divisor and Least Common Multiple}
\description{
  Calculates the greatest common divisor (GCD) and least common multiple (LCM).
}
\usage{
GCD(x)
LCM(x)

}
\arguments{
  \item{x}{a vector of integers.}
}
\details{
  The computation is based on the Euclidean algorithm without using the extended
  version.The greatest common divisor for
  all numbers in the integer vector \code{x} will be computed (the multiple GCD). 
}
\value{
  A numeric (integer) value.
}
\note{
  The following relation is always true:

  \code{n * m = GCD(n, m) * LCM(n, m)}
}
\author{Hans W Borchers <hwborchers@googlemail.com>\cr
combined by Andri Signorell <andri@signorell.net>}

\note{This functions stem from the library \pkg{numbers} and are a combination of \code{GCD}, \code{LCM}, \code{mGCD}, \code{mLCM}.}

\seealso{
\code{\link{Factorize}}, \code{\link{Primes}}
}
\examples{
GCD(c(12, 10))
GCD(c(46368, 75025))  # Fibonacci numbers are relatively prime to each other

LCM(c(12, 10))
LCM(c(46368, 75025))  # = 46368 * 75025

GCD(c(2, 3, 5, 7) * 11)
GCD(c(2*3, 3*5, 5*7))
LCM(c(2, 3, 5, 7) * 11)
LCM(c(2*3, 3*5, 5*7))
}
