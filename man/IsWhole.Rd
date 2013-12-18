\name{IsWhole}
\alias{IsWhole}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Is x a Whole Number?
%%  ~~function to do ... ~~
}
\description{Test if x contains integer numbers.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IsWhole(x, tol = .Machine$double.eps^0.5, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{tol}{tolerance to be used
%%     ~~Describe \code{tol} here~~
}
  \item{na.rm}{logical, indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE. 
%%     ~~Describe \code{na.rm} here~~
}

}
\details{
This is the suggested solution for doing the job, as is.integer tests for (class(x) == "integer") and does not test if x contains integer numbers. 
(Why not simply implement it in \pkg{base}?)
%%  ~~ If necessary, more details than the description above ~~
}
\value{logical vector of the same dimension as x.
}
\author{R-Core
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{is.integer}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
(x <- seq(1,5, by=0.5))
IsWhole( x ) #-->  TRUE FALSE TRUE ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ logic }

