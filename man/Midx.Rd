\name{Midx}
\alias{Midx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find the Midpoints of a Numeric Vector
%%  ~~function to do ... ~~
}
\description{Calculate the midpoints of a sequence of numbers.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Midx(x, first = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the numeric vector
%%     ~~Describe \code{x} here~~
}
  \item{first}{number which is to be inserted in the very beginning of the vector. Default is NULL, meaning nothing will be inserted. 
%%     ~~Describe \code{first} here~~
}
}
\value{numeric vector with the calculated midpoins
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\examples{
x <- c(1,3,6,7)
Midx(x)
Midx(x, 0)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
