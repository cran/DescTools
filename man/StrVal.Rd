\name{StrVal}
\alias{StrVal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract All Numbers From a String
%%  ~~function to do ... ~~
}
\description{Extract all numbers from a string, using a regular expression.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrVal(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector
%%     ~~Describe \code{x} here~~
}
}
\value{depending on the results the function will return either a character vector, in the case every element of x contained only one number, or a list of character vectors containing the found numbers.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{other string functions, e.g. \code{\link{StrTrunc}}
}
\examples{
# a simple vector with only one number per element
StrVal(x=c("week 1", "week 3", "week 4", "week 5"))

# several numbers per element
StrVal(x=c("This is 1. place: 45.2", "none", "12.1 but -2.7 follow, 10.2e23 "))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
