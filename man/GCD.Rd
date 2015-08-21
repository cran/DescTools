\name{GCD, LCM}
\alias{GCD}
\alias{LCM}

\title{Greatest Common Divisor and Least Common Multiple}
\description{
  Calculates the greatest common divisor (GCD) and least common multiple (LCM) of all the values present in its arguments.
}
\usage{
GCD(..., na.rm = FALSE)
LCM(..., na.rm = FALSE)

}
\arguments{
  \item{...}{integer or logical vectors.}
  \item{na.rm}{logical. Should missing values (including NaN) be removed?}
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
\author{Andri Signorell <andri@signorell.net>, \cr originally based on code in package \pkg{numbers} by Hans W Borchers <hwborchers@googlemail.com> }


\seealso{
\code{\link{Factorize}}, \code{\link{Primes}}
}
\examples{
GCD(12, 10)
GCD(46368, 75025)  # Fibonacci numbers are relatively prime to each other

LCM(12, 10)
LCM(c(46368, 75025))  # = 46368 * 75025

# all elements will be flattened by unlist
GCD(2, 3, c(5, 7) * 11)
GCD(c(2*3, 3*5, 5*7))
LCM(c(2, 3, 5, 7) * 11)
LCM(2*3, 3*5, 5*7)
}
