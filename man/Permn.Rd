\name{Permn}
\alias{Permn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Determine All Possible Permutations of a Set 
%%  ~~function to do ... ~~
}
\description{ Return the set of permutations for a given set of values. 
The values can be numeric values, characters or factors.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Permn(x, sort = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of numeric values or characters. Characters need not be unique.
%%     ~~Describe \code{x} here~~
}
  \item{sort}{logical, defining if the result set shouldb be sorted. Default is FALSE.
%%     ~~Describe \code{x} here~~
}
}
\details{The vector x need not contain unique values. The permutations will
automatically be filtered for unique sets, if the same element is given twice or more.}
\value{a data.frame with all possible permutations of the values in x. 
}
\author{Friederich Leisch <Friedrich.Leisch@boku.ac.at>, small changes Andri Signorell <andri@signorell.net> 
%%  ~~who you are~~
}

\seealso{\code{\link{combn}}, \code{\link{choose}}, \code{\link{factorial}}, \code{\link{GetAllSubsets}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Permn(letters[2:5])
Permn(2:5)

Permn(c("a","b","c","a"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
