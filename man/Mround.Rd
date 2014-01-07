\name{Mround}
\alias{Mround}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Round to Multiple
%%  ~~function to do ... ~~
}
\description{Returns a number rounded to the desired multiple.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Mround(x, multiple)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric. The value to round.
%%     ~~Describe \code{x} here~~
}
  \item{multiple}{numeric. The multiple to which the number is to be rounded.
%%     ~~Describe \code{multiple} here~~
}
}
\details{Mround rounds up, away from zero, if the remainder of dividing number by multiple is greater than or equal to half the value of multiple.
%%  ~~ If necessary, more details than the description above ~~
}
\value{the rounded value,
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

\seealso{\code{\link{round}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Mround(10, 3)     # Rounds 10 to a nearest multiple of 3 (9)
Mround(-10, -3)   # Rounds -10 to a nearest multiple of -3 (-9)

Mround(1.3, 0.2)  # Rounds 1.3 to a nearest multiple of 0.2 (1.4)
Mround(5, -2)     # Returns an error, because -2 and 5 have different signs    

Mround(c(1.92, 45.38, 0.831125), 0.05)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

