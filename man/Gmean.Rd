\name{Gmean}
\alias{Gmean}
\alias{Gsd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometric Mean and Standard Deviation
%%  ~~function to do ... ~~
}
\description{Calculates the geometric mean and the geometric standard deviation of a vector x.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Gmean(x, na.rm = FALSE)

Gsd(x, na.rm = FALSE)
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
The geometric mean and sd are restricted to positive inputs (because otherwise the answer can have an imaginary component).\cr
Use \code{\link{sapply}} to calculate the measures from data frame, resp. from a matrix. \cr
}
\value{a numeric value. 
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{mean}}, \code{\link{Hmean}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x <- runif(5)
Gmean(x)

m <- matrix(runif(50), nrow = 10)
apply(m, 2, Gmean)

sapply(as.data.frame(m), Gmean)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }

