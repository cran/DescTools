\name{GetAllSubsets}
\alias{GetAllSubsets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get All Subsets out of a List of Elements
%%  ~~function to do ... ~~
}
\description{Returns a list with all the subsets that can be built based on the elements given in x.\cr
The number of elements used in the combinations can be limited by setting min.n and max.n. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetAllSubsets(x, min.n = 1, max.n = length(x))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector with elements
%%     ~~Describe \code{x} here~~
}
  \item{min.n}{the minimum count of elements to be drawn. This defaults to 1.
%%     ~~Describe \code{min.n} here~~
}
  \item{max.n}{the maximum count of elements to be drawn. Default is "all elements in x".
%%     ~~Describe \code{max.n} here~~
}
}
\value{a list with the subsets.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{GetPairs}}, \code{\link{PairApply}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- LETTERS[1:6]

GetAllSubsets(x)

#get all sets of size 2 and 3
GetAllSubsets(x, min.n = 2, max.n = 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
