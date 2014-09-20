\name{Hmean}
\alias{Hmean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Harmonic mean
%%  ~~function to do ... ~~
}
\description{Calculates the harmonic mean of a vector x.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Hmean(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a positive numeric vector. An object which is not a vector is coerced (if possible) by as.vector.
%%     ~~Describe \code{x} here~~
}
  \item{na.rm}{logical, indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE. 
%%     ~~Describe \code{na.rm} here~~
}
}
\details{
If any argument is negative, then the result will be NA.
If any argument is zero, then the harmonic mean is zero. Otherwise, the harmonic mean is the reciprocal of the arithmetic mean of the reciprocals of the values.
\cr
Use \code{\link{sapply}} to calculate the measures from data frame, resp. from a matrix. \cr
}
\value{
a numeric value. 
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{Gmean}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- runif(5)
Hmean(x)

m <- matrix(runif(50), nrow = 10)
apply(m, 2, Hmean)

sapply(as.data.frame(m), Hmean)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }

