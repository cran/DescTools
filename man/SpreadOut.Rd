\name{SpreadOut}
\alias{SpreadOut}

\title{Spread out a vector of numbers to a minimum interval}
\description{
 Spread out a vector of numbers so that there is a minimum interval
 between any two numbers when in ascending or descending order.
}
\usage{
 SpreadOut(x, mindist)
}
\arguments{
 \item{x}{a numeric vector which may contain NAs.}
 \item{mindist}{the minimum interval between any two values when in ascending
  or descending order.}
}
\details{
 \code{SpreadOut} starts at or near the middle of the vector and increases the
 intervals between the ordered values. NAs are preserved. \code{SpreadOut}
 first tries to spread groups of values with intervals less than \code{mindist}
 out neatly away from the mean of the group. If this doesn't entirely succeed,
 a second pass that forces values away from the middle is performed.

 \code{SpreadOut} is currently used to avoid overplotting of axis tick labels
 where they may be close together.
}
\value{
 On success, the spread out values. If there are less than two valid values, the original vector is returned.
}
\note{ This function is borrowed from the package \pkg{plotrix} (\code{SpreadOut}) and has been
integrated here without logical changes. 
}

\author{Jim Lemon}
\examples{

 SpreadOut(c(1, 3, 3, 3, 3, 5), 0.2)
 SpreadOut(c(1, 2.5, 2.5, 3.5, 3.5, 5), 0.2)
 SpreadOut(c(5, 2.5, 2.5, NA, 3.5, 1, 3.5, NA), 0.2)

 # this will almost always invoke the brute force second pass
 SpreadOut(rnorm(10), 0.5)

 }
\keyword{misc}
