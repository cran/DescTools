\name{RoundTo}
\alias{RoundTo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Round to Multiple
%%  ~~function to do ... ~~
}
\description{Returns a number rounded to the desired multiple.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RoundTo(x, multiple = 1, FUN = round)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric. The value to round.
%%     ~~Describe \code{x} here~~
}
  \item{multiple}{numeric. The multiple to which the number is to be rounded. Default is 1.
%%     ~~Describe \code{multiple} here~~
}
\item{FUN}{the rounding function as character or as expression. Can be one out of \code{ceiling}, \code{round} (default) or \code{floor}. }
}
\details{RoundTo rounds up, away from zero, if the remainder of dividing number by multiple is greater than or equal to half the value of multiple if \code{FUN} is set to \code{round}.\cr
If \code{FUN} is set to \code{ceiling} it will always round up, and if set to \code{floor} it will always round down.
%%  ~~ If necessary, more details than the description above ~~
}
\value{the rounded value
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

\seealso{\code{\link{round}}, \code{\link{ceiling}}, \code{\link{floor}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
RoundTo(10, 3)     # Rounds 10 to a nearest multiple of 3 (9)
RoundTo(-10, -3)   # Rounds -10 to a nearest multiple of -3 (-9)

RoundTo(1.3, 0.2)  # Rounds 1.3 to a nearest multiple of 0.2 (1.2)
RoundTo(5, -2)     # Returns an error, because -2 and 5 have different signs

# Round down
RoundTo(c(1,-1) * 1.2335, 0.05, floor)
RoundTo(c(1,-1) * 1233.5, 100, floor)

# Round up
RoundTo(c(1,-1) * 1.2335, 0.05, ceiling)
RoundTo(c(1,-1) * 1233.5, 100, ceiling)

# Round towards zero
RoundTo(c(1,-1) * 1.2335, 0.05, trunc)
RoundTo(c(1,-1) * 1233.5, 100, trunc)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

