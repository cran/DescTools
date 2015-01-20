\name{StrVal}
\alias{StrVal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract All Numeric Values From a String
%%  ~~function to do ... ~~
}
\description{Extract all numeric values from a string, using a regular expression and return a list of the values. If there are several, the values can be either be pasted and/or casted from character vectors to numeric values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrVal(x, paste = FALSE, as.numeric = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector
%%     ~~Describe \code{x} here~~
}
\item{paste}{should separatetly extracted numbers be pasted together? This can be useful to reverse a prior format action. Default is FALSE.}
\item{as.numeric}{logical value, determining if the extracted values should be converted to a number or be returned as characters. Default is FALSE.}
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

# several numbers per element, extract each part, do not paste and return characters
StrVal(x=c("This is 1. place: 45.2", "none", "12.1 but -2.7 follow, 10.2e23 "), 
       paste = FALSE, as.numeric = FALSE)

# a typical use case for this function is to reverse a previously 
#   applied number format

x <- c(100000, 4564654632, -456463)
xf <- Format(x, big.mark="'")

StrVal(xf, paste = TRUE, as.numeric = TRUE)

StrVal(xf, paste = TRUE, as.numeric = FALSE)
StrVal(xf, paste = FALSE, as.numeric = TRUE)
StrVal(xf, paste = FALSE, as.numeric = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
\keyword{utilities}
