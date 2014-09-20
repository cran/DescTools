\name{Closest}
\alias{Closest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find the Closest Value 
%%  ~~function to do ... ~~
}
\description{Find the closest value of a number in a vector x.}
\usage{
Closest(x, a, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the vector to be searched in
%%     ~~Describe \code{x} here~~
}
  \item{a}{the reference value
%%     ~~Describe \code{a} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
}
\value{the value in x which is closest to a
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{which}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(8)
x <- sample(10, size=10, replace=TRUE)

Closest(x, 6)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ logic }% __ONLY ONE__ keyword per line
