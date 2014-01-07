\name{StrCap}
\alias{StrCap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Capitalize the First Letter of a String}
\description{
Capitalize the first letter of each element of the string vector.
}
\usage{
StrCap(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ String to be capitalized. }
}
\value{
  Returns a vector of charaters with the first letter capitalized
}
\author{ Charles Dupont  <charles.dupont@vanderbilt.edu>}
\examples{
StrCap(c("Hello", "bob", "daN"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ character }% __ONLY ONE__ keyword per line
