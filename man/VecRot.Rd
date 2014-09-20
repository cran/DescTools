\name{VecRot}
\alias{VecRot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Vector Rotation
%%  ~~function to do ... ~~
}
\description{Shift the elements of a vector in circular mode to the right or to the left by \code{n} elements, such that the nth element is the first one of the new vector and the first n-1 elements are appended to the end.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VecRot(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of any type.
%%     ~~Describe \code{x} here~~
}
  \item{n}{the number of elements to shift.
%%     ~~Describe \code{n} here~~
}
}
\details{The function will repeat the vector two times and select the appropriate number of elements from the required shift on.

%%  ~~ If necessary, more details than the description above ~~
}
\value{the shifted vector in the same dimensions as x.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{[}}, \code{\link{rep}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
VecRot(c(1,1,0,0,3,4,8), 3)

VecRot(letters[1:10], 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}
