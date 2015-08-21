\name{StrAlign}
\alias{StrAlign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Align Strings To a Specified Character
%%  ~~function to do ... ~~
}
\description{Align several strings to the first occurance of a specified character.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StrAlign(x, sep = " ", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a character vector to be aligned.
%%     ~~Describe \code{x} here~~
}
  \item{sep}{the character on whose position the strings will be aligned. Left alignment can be defined by setting \code{sep = "\\\\^"} and right alignment by \code{"\\\\$"}.
%%     ~~Describe \code{sep} here~~
}
  \item{\dots}{the dots are being sent via \code{StrPos} to \code{\link{regexpr}} and can for example be used to set \code{fix = TRUE} for using plain text of the required pattern.
%%     ~~Describe \code{\dots} here~~
}
}
\value{a character vector
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{StrTrim}}, \code{\link{Format}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# align on (the first occuring) B
x <- c("ABCDMNB", "CDGHEBK", "BCI")
cbind(StrAlign(x, sep="B"))

# align to decimal point
z <- c("    6.0", "6.00  ", " 45.12    ")
cbind(StrAlign(z, sep="."))

# right align, the width will be the max number of characters in x
StrAlign(x, sep="\\\\$")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{character}
