\name{DenseRank}
\alias{DenseRank}
\alias{PercentRank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dense Ranks and Percent Ranks
%%  ~~function to do ... ~~
}
\description{Computes the rank of a vector and returns the rank.
DenseRank calculate consecutive ranks in the case of present ties.. No ranks are skipped if there are ranks with multiple items. (Unlike \code{\link{rank}} gives the ranking within the vector x too, but ties are assigned the same rank, with the next ranking(s) skipped.)

DENSE_RANK computes the rank of a row in an ordered group of rows and returns the rank as a NUMBER. The ranks are consecutive integers beginning with 1. The largest rank value is the number of unique values returned by the query. Rank values are not skipped in the event of ties. Rows with equal values for the ranking criteria receive the same rank. This function is useful for top-N and bottom-N reporting
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DenseRank(x, na.last = TRUE)

PercentRank(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric, complex, character or logical vector.
%%     ~~Describe \code{x} here~~
}
  \item{na.last}{for controlling the treatment of \code{\link{NA}}s. If \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they are put first; if \code{NA}, they are removed; if \code{"keep"} they are kept with rank \code{NA}.
%%     ~~Describe \code{na.last} here~~
}
}
\value{A numeric vector of the same length as x with names copied from x (unless \code{na.last = NA}, when missing values are removed). The vector is of integer type unless x is a long vector.
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

\seealso{\code{\link{rank}}, \code{\link{factor}}, \code{\link{order}}, \code{\link{sort}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))

x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:11]
(r2 <- rank(x2))        # ties are averaged
(r2 <- DenseRank(x2))   # ranks are enumerated

PercentRank(x2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
